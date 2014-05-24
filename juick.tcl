namespace eval juick {}

# Start this procedure at source file
juick::preload

# XRDB options
option add *juick.number                blue           widgetDefault
option add *juick.private.fg            blue           widgetDefault
option add *juick.private.bg            #FF9A15        widgetDefault
option add *juick.citing                gray35         widgetDefault
option add *juick.nick                  red            widgetDefault
option add *juick.tag                   ForestGreen    widgetDefault
option add *juick.my_msg                gray           widgetDefault

# list of {tag_name xrdb_name xrdb_option}
# used in 'configurator' proc
variable juick::richtext_tags {
    {juick_number  juick.number     foreground}
    {juick_private juick.private.fg foreground}
    {juick_private juick.private.bg background}
    {juick_citing  juick.citing     foreground}
    {juick_nick    juick.nick       foreground}
    {juick_tag     juick.tag        foreground}
    {juick_my_msg  juick.my_msg     foreground}
}

variable juick::options
variable juick::nicknames

# for tab completition, will be removed soon
variable juick::chat_things

# See commented code in juick_commands_comps, variable currently unused
# variable juick::commands {HELP NICK LOGIN "S " "U " ON OFF "D " "BL " "WL " "PM " VCARD PING INVITE}
# or
# variable juick::commands {HELP NICK LOGIN S U ON OFF D BL WL PM CARD PING INVITE}

# TODO: reconstructor for md urls
# list of {name opts priority}
# where 'opts' is some list of:
# {configurator, parser, reconstructor, renderer}
variable juick::richtext_parsers { 
    {juick_md_url_square_brackets {parser}                 49}
    {juick_md_url_round_brackets  {parser}                 49}
    {juick_number                 {parser}                 54}
    {juick_private                {parser}                 81}
    {juick_citing                 {parser}                 82}
    {juick_nicks_tags             {parser}                 85}
    {juick_common                 {configurator, renderer} 85}
}

# list of {hook proc priority orig_proc}
variable juick::plugin_hooks {
    {roster_push_hook          determine_juick_nick       99 ""}
    {draw_message_hook         ignore_server_messages      0 ""}
    {draw_message_hook         draw_message_handle        21 ""}
    {chat_window_click_hook    insert_by_click            99 ""}
    {chat_win_popup_menu_hook  add_chat_things_menu       20 ""}
    {rewrite_message_hook      rewrite_incoming_message   20 ""}
    {chat_send_message_hook    rewrite_subscribe_plus_cmd 19 ""}
    {generate_completions_hook juick_commands_comps       99 ""}
    {draw_message_hook         update_juick_tab            8 \
        ::plugins::update_tab::update}
    {draw_message_hook         add_number_to_tab_title    18 \
        ::::ifacetk::add_number_of_messages_to_title}
}

proc juick::preload {} {
    package require msgcat
    package require http

    # Compatibility with old Tkabber version
    if {$::tkabber_version eq "0.11.1"} {
        set scriptdir [file dirname [info script]]
        set scriptname tkabber-0.11.1-compatibility.tcl 
        source [file join $scriptdir $scriptname]
    }

    ::msgcat::mcload [file join [file dirname [info script]] msgs]

    # Register plugin for further load/unload via GUI
    if {![::plugins::is_registered juick]} {
        ::plugins::register juick \
            -namespace [namespace current] \
            -source [info script] \
            -description [::msgcat::mc \
                "Whether the Juick plugin is loaded."] \
            -loadcommand [namespace code load] \
            -unloadcommand [namespace code unload]
        return
    }

    # Create 'Customize -> Plugins -> Juick' menu
    custom::defgroup Plugins [::msgcat::mc "Plugins options."] \
        -group Tkabber
    custom::defgroup Juick [::msgcat::mc "Juick settings."] \
        -group Plugins
    custom::defvar options(main_jid) "juick@juick.com/Juick" \
        [::msgcat::mc "Main Juick JID. This used for \
            forwarding things from other chats."] \
        -group Juick -type string
    custom::defvar options(special_update_juick_tab) 1 \
        [::msgcat::mc "Indicate as personal message only private \
            messages and replies to you.\nYour Juick nickname \
            determines at roster receiving, so after enable option \
            you need to reconnecting."] \
        -group Juick -type boolean
}

proc juick::load {} {
    variable richtext_parsers
    variable plugin_hooks

    foreach {name opts priority} $richtext_parsers {
        ::richtext::entity_state $name 1

        set args {}
        foreach opt $opts {
            if {[lsearch -exact $opt {configurator parser}] >= 0} {
                lappend args -$opt [list [namespace current]::$opt $name]
            }
        }

        foreach opt $opts {
            if {[lsearch -exact $opt {reconstructor renderer}] >= 0} {
                lappend args -$opt [namespace current]::$opt
            }
        }

        lappend args -parser-priority $priority
        ::richtext::register_entity $name $args
    }

    foreach {hook proc priority orig_proc} $plugin_hooks {
        if {ne $orig_proc ""} {
            hook::remove $orig_hook $priority
        }

        hook::add $hook [namespace current]::$proc $priority
    }

    set scriptdir [file dirname [info script]]
    source [file join $scriptdir utils.tcl]
}

proc juick::unload {} {
    variable richtext_parsers
    variable plugin_hooks

    foreach {name _ _} $richtext_parsers {
        ::richtext::unregister_entity $name
        ::richtext::enity_state $name 0
    }

    foreach {hook proc priority orig_proc} $plugin_hooks {
        hook::remove $hook [namespace current]::$proc $priority

        if {$orig_proc ne ""} {
            hook::add $orig_hook $priority
        }
    }
}

proc juick::determine_juick_nick {xlib jid name groups subsc ask} {
    variable options
    variable nicknames

    if {![info exists options(special_update_juick_tab)] \
        || ! $options(special_update_juick_tab)} \
    {
        return
    }

    if {![is_juick_jid $jid] || [info exists nicknames($jid)]} {
        return
    }

    # TODO
    # For Juick contacts connected via j2j transport is difficult
    # to determine user jid.
    # Idea: send iq register to proper j2j, get form, and read
    # second JID from it.
    if {[string first % $jid] >= 0} {
        return
    }

    set my_jid [connection_jid $xlib]
    set my_jid [::xmpp::jid::removeResource $my_jid]
    set nick_request_url "http://api.juick.com/users?jid=$my_jid"

    if {[catch {::http::geturl $nick_request_url} token]} {
        return
    }

    # Data format (json):
    # [{"uid":XXXX,"uname":"XXXX","jid":"XXXX@XXXX"}]
    set data [::http::data $token]
    ::http::cleanup $token

    # If has no json package, try to parse by regexp.
    if {[catch {package require json}]} {
        if {![regexp {\[{.*"uname"\w*:\w*"([^"]+)".*}\]} $data -> uname]} {
            # For vim syntax highlighter: "
            return
        }

        set nicknames($jid) $uname
        return
    }

    set ds [::json::json2dict $data]

    if {[llength $ds] != 1} {
        return
    }

    set d [lindex $ds 0]

    # If dict command not available...
    if {[catch {dict create}]} {
        # Try to load dict package...
        if {[catch {package require dict}]} {
            # If it fails, get uname value without dict command.
            foreach {key value} $d {
                switch -- $key {
                    uname { set uname $value }
                }
            }

            set nicknames($jid) $uname
            return
        }
    }

    if {[catch {dict get $d uname} uname]} {
        return
    }

    set nicknames($jid) $uname
}

proc juick::draw_message_handle {chatid from type body x} {
    if {![is_juick $chatid]} return

    ::richtext::property_add {JUICK} {}

    set chatw [chat::chat_win $chatid]
    set jid [chat::get_jid $chatid]

    if {$jid eq $from} {
        ::richtext::render_message $chatw $body {JUICK_MY_MSG}
    } else {
        ::richtext::render_message $chatw $body {}
    }

    return stop
}

proc juick::update_juick_tab {chatid from type body x} {
    variable options
    if {!([is_juick_jid $from] && ($type eq "chat") \
        && $options(special_update_juick_tab))} \
    {
        ::plugins::update_tab::update $chatid $from $type $body $x
        return
    }

    # See ${PATH_TO_TKABBER}/plugins/chat/update_tab.tcl
    foreach xelem $x {
        ::xmpp::xml::split $xelem tag xmlns attrs cdata subels
        if {($tag eq "") && ($xmlns eq "tkabber:x:nolog")} \
        {
            return
        }
    }

    set cw [chat::winid $chatid]

    if {[is_personal_juick_message $from $body]} {
        tab_set_updated $cw 1 mesg_to_user
    } else {
        tab_set_updated $cw 1 message
    }
}

proc juick::ignore_server_messages {chatid from type body x} {
    if {[is_juick $chatid] && $from eq ""} {
        return stop
    }
}

# Add number of messages from Juick to the tab title
proc juick::add_number_to_tab_title {chatid from type body x} {
    variable options
    if {!([is_juick_jid $from] && ($type eq "chat") \
        && $options(special_update_juick_tab))} \
    {
        ::ifacetk::add_number_of_messages_to_title $chatid $from $type \
            $body $x
        return
    }

    # See ${PATH_TO_TKABBER}/ifacetk/iface.tcl
    foreach xelem $x {
        ::xmpp::xml::split $xelem tag xmlns attrs cdata subels
        if {($tag eq "") && ($xmlns eq "tkabber:x:nolog")} \
        {
            return
        }
    }

    if {[::ifacetk::chat_window_is_active $chatid]} return
    if {$from eq ""} return

    variable ::ifacetk::number_msg
    variable ::ifacetk::personal_msg

    incr number_msg($chatid)

    if {[is_personal_juick_message $from $body]} {
        incr personal_msg($chatid)
    }

    ::ifacetk::update_chat_title $chatid
    ::ifacetk::update_main_window_title
}

proc juick::rewrite_incoming_message \
     {vxlib vfrom vid vtype vis_subject vsubject \
      vbody verr vthread vpriority vx} {
    upvar 2 $vfrom from
    upvar 2 $vtype type
    upvar 2 $vbody body
    upvar 2 $vx x

    if {![is_juick_jid $from] || ($type ne "chat")} {
        return
    }

#############################
# Remove jabber:x:oob element
    set newx {}

    foreach xe $x {
        ::xmpp::xml::split $xe tag xmlns attrs cdata subels

        if {$xmlns ne "jabber:x:oob"} {
            lappend newx $xe
        }
    }

    set x $newx

#############################
# Add GMT time
    foreach xe $x {
        ::xmpp::xml::split $xe tag xmlns attrs cdata subels

        if {$xmlns eq "http://juick.com/message"} {
            foreach {key val} $attrs {
                if {$key eq "ts"} {
                    set body "$val GMT\n$body"
                }
            }
        }
    }
}

proc juick::rewrite_subscribe_plus_cmd {chatid user body type} {
    if {![is_juick $chatid] || ($type ne "chat")} {
        return
    }

    if {[regexp {^[Ss] (#\d+)\+\s*$} $body -> thing]} {
        set xlib [chat::get_xlib $chatid]
        set jid [chat::get_jid $chatid]

        chat::add_message $chatid $user $type $body {}
        message::send_msg $xlib $jid -type chat -body "S $thing"
        message::send_msg $xlib $jid -type chat -body "$thing+"

        return stop
    }
}

proc juick::insert_by_click {chatid w x y} {
    variable options
    set thing ""
    set cw [chat::chat_win $chatid]
    set ci [chat::input_win $chatid]
    set jid [::xmpp::jid::removeResource [chat::get_jid $chatid]]

    set tags [$cw tag names "@$x,$y"]

    if {[set idx [lsearch -glob $tags JUICK-*]] >= 0} {
        set thing [string range [lindex $tags $idx] 6 end]
    }

    if {$thing eq ""} return

    if {![is_juick_jid $jid]} {
        set xlib [chat::get_xlib $chatid]
        set mainchat [chat::chatid $xlib $options(main_jid)]

        if {[chat::is_opened $mainchat]} {
            chat::activate $mainchat
        } else {
            chat::open_to_user $xlib $options(main_jid)
        }

        set ci [chat::input_win $mainchat]
    }

    $ci insert insert "$thing "
    focus -force $ci
    return stop
}

proc juick::add_chat_things_menu {m chatwin X Y x y} {
    set thing ""
    set tags [$chatwin tag names "@$x,$y"]

    if {[set idx [lsearch -glob $tags JUICK-*]] >= 0} {
        set thing [string range [lindex $tags $idx] 6 end]
    }

    if {$thing eq ""} return

    $m add command -label [format [::msgcat::mc \
        "\[J\] Copy %s to clipboard"] $thing] \
        -command [list [namespace current]::copy_thing $chatwin $thing]

    $m add command -label [::msgcat::mc \
        "\[J\] Open in browser"]\
        -command [list [namespace current]::browse_thing $chatwin $thing]

    if {[regexp {#\d+$} $thing]} {
####################
        # Currently not implemented.
        if {0} {
            $m add command -label [::msgcat::mc \
                "\[J\] Open in new tab"] \
                -command [list [namespace current]::get_juick_thread \
                $chatwin $thing]
        }
####################

        $m add command -label [::msgcat::mc \
            "\[J\] Subscribe"] \
            -command [list [namespace current]::send_to_juick \
            $chatwin "S $thing"]

        $m add command -label [::msgcat::mc \
            "\[J\] Unsubscribe"] \
            -command [list [namespace current]::send_to_juick \
            $chatwin "U $thing"]
    }
}

# TODO: use [$t tag ranges JUICK]
# or [$t tag prevrange JUICK] in replaced 'complete' proc.
proc juick::juick_commands_comps {chatid compsvar wordstart line} {
    if {![is_juick $chatid]} return

    upvar 0 $compsvar comps
    variable chat_things
    variable commands

    if {!$wordstart} {
       set comps [concat $commands $comps]
    } else {
####################
        # Currently not implemented.
        if {0} {
            # This code don't work.
            # See ${PATH_TO_TKABBER}/plugins/chat/completion.tcl at line 94.
            # Idea: use *rename* for procedure completion::complete.
            set q 0
            foreach cmd $commands {
                if {[string equal -length [string length $cmd] $cmd $line]} {
                    set q 1
                    break
                }
            }

            if {!$q} return
        }
####################
    }

    if {[info exist chat_things($chatid)]} {
       set comps [concat $chat_things($chatid) $comps]
    }
}

# RichText stuff
# --------------

proc juick::configurator {w} {
    foreach {tag_name xrdb_name xrdb_option} $xrdb_options {
        set options($name) [option get $w $name Text]
        $w tag configure $tag -$opt $options($name)
    }

    # Display all things and citing in our message with 'my message' colors.
    $w tag raise my_msg juick_citing

    # Display all things in citing with citing colors.
    # Priorities not affect clickableness.
    # Example:
    # > Text text #1234 @nick *tag text
    $w tag raise juick_citing juick_number
    $w tag raise juick_citing juick_nick
    $w tag raise juick_citing juick_tag

    # Just for certain and predictable priority.
    $w tag raise juick_number juick_private
    $w tag raise juick_nick   juick_private
    $w tag raise juick_tag    juick_private
}

proc juick::parser_spot_md_url {type what at startVar endVar urlInfoVar} {
    variable ::plugins::urls::url_regexp
    upvar 1 urlInfoVar urlInfo
 
    switch -exact $type {
        juick_md_url_square_brackets {
            # [title][URL]
            set md_regexp {(\[([^\]]+)\]\[([^\]]+)\])}
        }
        juick_md_url_round_brackets {
            # [title](URL)
            set md_regexp {(\[([^\]]+)\]\(([^\)]+)\))}
        }
    }

    set matched [regexp -indices -start $at -- \
        $md_regexp $what -> \
        bounds title_bounds url_bounds]

    if {!$matched} { return false }

    lassign $title_bounds ts te
    lassign $url_bounds us ue
    set title [string range $what $ts $te]
    set url [string range $what $us $ue]

    set md_url_regexp [format "^%s$" $::plugins::urls::url_regexp]
    set matched [regexp -expanded -nocase -- $md_url_regexp $url]

    if {!$matched} { return false }

    upvar 1 $startVar uStart $endVar uEnd
    lassign $bounds uStart uEnd

    lset urlInfo $title $url

    return true
}

proc juick::parser_spot {ptype what at startVar endVar typeVar urlInfoVar} {
    upvar 1 $startVar uStart $endVar uEnd
    upvar 1 typeVar type urlInfoVar urlInfo
    set urlInfo {}

    switch -glob $ptype {
        juick_md_url* {
            return [spot_md_url $ptype $what $at uStart uEnd typeVar urlInfo]
        }
        juick_number {
            set re {(?:\s|\n|\A|\(|\>)(#\d+(/\d+)?)(?:(\.(\s|\n))?)}
        }
        juick_private {
            set re {(^Private message)(?: from @.+:\n)}
        }
        juick_citing {
            set re {(?:\n|\A)(>[^\n]+)}
        }
        juick_nicks_tags {
            set re {(?:\s|\n|\A|\(|\>)(@[\w@.-]+|\*[\w?!+'/.-]+)(?:(\.(\s|\n))?)}
        }
    }

    set matched [regexp -indices -start $at -- $re $what -> bounds]
    if {!$matched} { return false }
    lassign $bounds uStart uEnd
    set thing [string range $s $uStart $uEnd]

    if {$ptype eq juick_nicks_tags} {
        switch -exact [string index $thing 0] {
            "@" { set type juick_nick   }
            "*" { set type juick_tag    }
        }
    } else {
        set type $ptype
    }

    return true
}

proc juick::parser_write {ptype thing tags urlInfo outVar} {
    upvar 1 outVar out
    lassign $urlInfo title url

    if {[string match juick_md_url* $ptype]} {
        lappend out $url url $tags
        ::richtext::property_update url:title,$url $title
    } else {
        lappend out $thing $ptype $tags
    }
}

proc juick::parser {ptype atLevel accName} {
    upvar #$atLevel $accName chunks

    if {![::richtext::property_exists {JUICK}]} { return }

    set out {}

    foreach {s type tags} $chunks {
        if {[lsearch -exact {text juick_citing) $type] == -1} {
            # pass through
            lappend out $s $type $tags
            continue
        }

        set index 0; set uStart 0; set uEnd 0

        while {[eval {parser_spot $ptype $s $index uStart uEnd urlInfo}]} {
            # Write out text before current thing, if exists
            if {$uStart - $index > 0} {
                set text_before [string range $s $index [expr {$uStart - 1}]]
                lappend out $text_before $type $tags
            }

            set thing [string range $s $uStart $uEnd]
            lappend tags $ptype
            # Write out current thing
            parser_write $ptype $thing $tags $urlInfo out
            set index [expr {$uEnd + 1}]
        }

        # Write out text after the last thing, if exists
        if {[string length $s] - $index > 0} {
            set text_after [string range $s $index end]
            lappend out $text_after $type $tags
        }
    }

    set chunks $out
}

proc juick::renderer {w type piece tags} {
    $w insert end $piece $tags
}

# vi:ts=4:et
