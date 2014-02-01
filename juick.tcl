package require msgcat
package require http

option add *juick.nick                  red            widgetDefault
option add *juick.tag                   ForestGreen    widgetDefault
option add *juick.my                    gray           widgetDefault
option add *juick.number                blue           widgetDefault
option add *juick.private_foreground    blue           widgetDefault
option add *juick.private_background    #FF9A15        widgetDefault
option add *juick.citing                gray35         widgetDefault

if {[string equal $::tkabber_version "0.11.1"]} {
    set scriptdir [file dirname [info script]]

    catch {source [file join $scriptdir \
        tkabber-0.11.1-compatibility.tcl]} \
        source_compatibility_file_result
}

namespace eval juick {
variable options
variable juick_nicknames
variable chat_things

::msgcat::mcload [file join [file dirname [info script]] msgs]

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

custom::defgroup Plugins [::msgcat::mc "Plugins options."] -group Tkabber

set group "Juick"
custom::defgroup $group \
    [::msgcat::mc "Juick settings."] \
    -group Plugins

custom::defvar options(main_jid) "juick@juick.com/Juick" \
    [::msgcat::mc \
        "Main Juick JID. This used for forwarding things from other chats."] \
    -group $group \
    -type string
custom::defvar options(special_update_juick_tab) 1 \
    [format "%s\n%s" \
        [::msgcat::mc "Indicate as personal message only private messages and replies to you."] \
        [::msgcat::mc "Your Juick nickname determines at roster receiving, so after enable option you need to reconnecting."]] \
    -group $group \
    -type boolean

proc load {} {
    ::richtext::entity_state juick_numbers 1
    ::richtext::entity_state citing 1
    ::richtext::entity_state juick 1
    ::richtext::entity_state juick_ligth 1

    hook::add draw_message_hook        \
        [namespace current]::ignore_server_messages 0

    hook::add draw_message_hook        \
        [namespace current]::handle_message 21

    hook::add chat_window_click_hook   \
        [namespace current]::insert_from_window

    hook::add chat_win_popup_menu_hook \
        [namespace current]::add_juick_things_menu 20

    hook::add rewrite_message_hook     \
        [namespace current]::rewrite_juick_message 20

    hook::add chat_send_message_hook   \
        [namespace current]::rewrite_send_juick_message 19

    hook::add draw_message_hook \
        [namespace current]::update_juick_tab 8

    hook::remove draw_message_hook ::plugins::update_tab::update 8

    hook::add draw_message_hook \
        [namespace current]::add_number_of_messages_from_juick_to_title 18

    hook::remove draw_message_hook \
        ::::ifacetk::add_number_of_messages_to_title 18

    hook::add roster_push_hook \
        [namespace current]::get_juick_nick 99

    hook::add generate_completions_hook \
        [namespace current]::juick_commands_comps 99
}

proc unload {} {
    hook::remove draw_message_hook        \
        [namespace current]::ignore_server_messages 0

    hook::remove draw_message_hook        \
        [namespace current]::handle_message 21

    hook::remove chat_window_click_hook   \
        [namespace current]::insert_from_window

    hook::remove chat_win_popup_menu_hook \
        [namespace current]::add_juick_things_menu 20

    hook::remove rewrite_message_hook     \
        [namespace current]::rewrite_juick_message 20

    hook::remove chat_send_message_hook   \
        [namespace current]::rewrite_send_juick_message 19

    hook::remove draw_message_hook \
        [namespace current]::update_juick_tab 8

    hook::add draw_message_hook \
        ::plugins::update_tab::update 8

    hook::remove draw_message_hook \
        [namespace current]::add_number_of_messages_from_juick_to_title 18

    hook::add draw_message_hook \
        ::::ifacetk::add_number_of_messages_to_title 18

    hook::remove roster_push_hook \
        [namespace current]::get_juick_nick 99

    hook::remove generate_completions_hook \
        [namespace current]::juick_commands_comps 99

    ::richtext::entity_state juick_numbers 0
    ::richtext::entity_state citing 0
    ::richtext::entity_state juick 0
    ::richtext::entity_state juick_ligth 0
}

proc is_juick_jid {jid} {
    set jid [::xmpp::jid::removeResource $jid]
    set node [::xmpp::jid::node $jid]
    return [expr {[string equal $jid "juick@juick.com"] || \
        [string equal $node "juick%juick.com"]}]

#    if {$without_resource} { \
#        return [expr {[string equal $jid "juick@juick.com"] \
#           || [regexp "juick%juick.com@.+" $jid]}] \
#    } else { \
#       return [expr {[string equal $jid "juick@juick.com/Juick"] \
#           || [regexp "juick%juick.com@.+/Juick" $jid]}] \
#    }
}

# Determines whether given chatid correspond to Juick
proc is_juick {chatid} {
    set jid [chat::get_jid $chatid]
    return [is_juick_jid $jid]
}

proc get_juick_nick {xlib jid name groups subsc ask} {
    variable options
    variable juick_nicknames

    if {![info exists options(special_update_juick_tab)] \
        || ! $options(special_update_juick_tab)} \
    {
        return
    }

    if {![is_juick_jid $jid] || [info exists juick_nicknames($jid)]} {
        return
    }

    # For Juick contacts connected via j2j transport is difficult
    # to determine user jid.
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

        set juick_nicknames($jid) $uname
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

            set juick_nicknames($jid) $uname
            return
        }
    }

    if {[catch {dict get $d uname} uname]} {
        return
    }

    set juick_nicknames($jid) $uname
}

proc handle_message {chatid from type body x} {
    if {![is_juick $chatid]} return

    ::richtext::property_add {JUICK} {}

    set chatw [chat::chat_win $chatid]
    set jid [chat::get_jid $chatid]

    set tags {}
    if {![string equal $jid $from]} {
        lappend tags JMY
    }

    ::richtext::render_message $chatw $body $tags
    return stop
}

proc get_my_juick_nickname {jid} {
    variable juick_nicknames

    set uname ""
    set jid [::xmpp::jid::removeResource $jid]

    if {[info exists juick_nicknames($jid)]} {
        set uname $juick_nicknames($jid)
    }

    return $uname
}

proc is_personal_juick_message {from body} {
    variable options

    set private_msg [regexp {^Private message from @.+:\n} $body]

    set reply_to_comment [regexp \
        {Reply by @[^\n ]+:\n>.+\n\n@([^\n ]+) .+\n\n#\d+/\d+ http://juick.com/\d+#\d+$} \
        $body -> reply_to_nick]

    if {$reply_to_comment} {
        set reply_to_me [string equal \
            [get_my_juick_nickname $from] $reply_to_nick]
    } else {
        set reply_to_me 0
    }

    return [expr {$private_msg || $reply_to_me}]
}

proc update_juick_tab {chatid from type body x} {
    variable options
    if {!([is_juick_jid $from] && [string equal $type "chat"] \
        && $options(special_update_juick_tab))} \
    {
        ::plugins::update_tab::update $chatid $from $type $body $x
        return
    }

    # See ${PATH_TO_TKABBER}/plugins/chat/update_tab.tcl
    foreach xelem $x {
        ::xmpp::xml::split $xelem tag xmlns attrs cdata subels
        if {[string equal $tag ""] && \
            [string equal $xmlns tkabber:x:nolog]} \
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

proc ignore_server_messages {chatid from type body x} {
    if {[is_juick $chatid] && $from eq ""} {
        return stop
    }
}

proc add_number_of_messages_from_juick_to_title {chatid from type body x} {
    variable options
    if {!([is_juick_jid $from] && [string equal $type "chat"] \
        && $options(special_update_juick_tab))} \
    {
        ::ifacetk::add_number_of_messages_to_title $chatid $from $type \
            $body $x
        return
    }

    # See ${PATH_TO_TKABBER}/ifacetk/iface.tcl
    foreach xelem $x {
        ::xmpp::xml::split $xelem tag xmlns attrs cdata subels
        if {[string equal $tag ""] && \
            [string equal $xmlns tkabber:x:nolog]} \
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

proc rewrite_juick_message \
     {vxlib vfrom vid vtype vis_subject vsubject \
      vbody verr vthread vpriority vx} {
    upvar 2 $vfrom from
    upvar 2 $vtype type
    upvar 2 $vbody body
    upvar 2 $vx x

    if {![is_juick_jid $from] || ![string equal $type "chat"]} {
        return
    }

#############################
# Remove jabber:x:oob element
    set newx {}

    foreach xe $x {
        ::xmpp::xml::split $xe tag xmlns attrs cdata subels

        if {![string equal $xmlns "jabber:x:oob"]} {
            lappend newx $xe
        }
    }

    set x $newx

#############################
# Add GMT time
    foreach xe $x {
        ::xmpp::xml::split $xe tag xmlns attrs cdata subels

        if {[string equal $xmlns "http://juick.com/message"]} {
            foreach {key val} $attrs {
                if {[string equal $key "ts"]} {
                    set body "$val GMT\n$body"
                }
            }
        }
    }
}

proc rewrite_send_juick_message {chatid user body type} {
    if {![is_juick $chatid] || ![string equal $type "chat"]} {
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

proc insert_from_window {chatid w x y} {
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

proc add_juick_things_menu {m chatwin X Y x y} {
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
# Currently not implemented.
if {0} {
        $m add command -label [::msgcat::mc \
            "\[J\] Open in new tab"] \
            -command [list [namespace current]::get_juick_thread \
            $chatwin $thing]
}

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

proc send_to_juick {w body} {
    variable options
    set cw [join [lrange [split $w .] 0 end-1] .]
    set chatid [chat::winid_to_chatid $cw]
    set xlib [chat::get_xlib $chatid]
    set jid [chat::get_jid $chatid]

    if {![is_juick_jid $jid]} {
        set mainchat [chat::chatid $xlib $options(main_jid)]
        set jid [chat::get_jid $mainchat]
    }

    message::send_msg $xlib $jid -type chat -body $body
}

proc get_juick_thread {w thing} {
    set cw [join [lrange [split $w .] 0 end-1] .]
    set chatid [chat::winid_to_chatid $cw]
    set xlib [chat::get_xlib $chatid]
    set jid [chat::get_jid $chatid]
    set mid [string range $thing 1 end]

    ::xmpp::sendIQ $xlib get \
        -query [::xmpp::xml::create query \
            -xmlns "http://juick.com/query#messages" \
            -attrs [list mid $mid]] \
        -to $jid \
        -command [list [namespace current]::receive_juick_thread $jid]
}

proc receive_juick_thread {jid res child0} {
    if {![string equal $res ok]} return

    ::xmpp::xml::split $child0 tag0 xmlns0 attrs0 cdata0 subels0

    if {![string equal $xmlns0 "http://juick.com/query#messages"]} return

    set child1 [lindex $subels0 0]
    ::xmpp::xml::split $child1 tag1 xmlns1 attrs1 cdata1 subels1

    if {![string equal $xmlns1 "http://juick.com/message"]} return

    set msg ""

    foreach child2 $subels1 {
        ::xmpp::xml::split $child2 tag2 xmlns2 attrs2 cdata2 subels2
        switch -- $tag2 {
            body {
                set msg $cdata2
            }
        }
    }

    #puts "Get message from $jid: \"$msg\""

    # open new tab

    return
}

proc copy_thing {w thing} {
    clipboard clear -displayof $w
    clipboard append -displayof $w $thing
}

proc browse_thing {w thing} {
    switch -regexp -- $thing {
        {^#} {
          regsub -- "/" [string range $thing 1 end] "#" jurl
          browseurl http://juick.com/$jurl
          }
        {^@} {
          browseurl http://juick.com/[string range $thing 1 end]
          }
        {^\*} {
          browseurl http://juick.com/tag/[string range $thing 1 end]
          }
    }
}

# See commented code in juick_commands_comps.
#variable commands {HELP NICK LOGIN "S " "U " ON OFF "D " "BL " "WL " "PM " VCARD PING INVITE}
variable commands {HELP NICK LOGIN S U ON OFF D BL WL PM CARD PING INVITE}
proc correct_command {chatid user body type} {
   # Maybe once I'll get arount to it
}

proc juick_commands_comps {chatid compsvar wordstart line} {
    if {![is_juick $chatid]} return

    upvar 0 $compsvar comps
    variable chat_things
    variable commands

    if {!$wordstart} {
       set comps [concat $commands $comps]
    } else {
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
    }

    if {[info exist chat_things($chatid)]} {
       set comps [concat $chat_things($chatid) $comps]
    }
}

# --------------
# RichText stuff

proc configure_juick {w} {
    set options(juick.nick) [option get $w juick.nick Text]
    set options(juick.tag) [option get $w juick.tag Text]
    set options(juick.my) [option get $w juick.my Text]

    $w tag configure JNICK -foreground $options(juick.nick)
    $w tag configure JTAG -foreground $options(juick.tag)
    $w tag configure JMY -foreground $options(juick.my)
}

proc configure_juick_numbers {w} {
    set options(juick.number) [option get $w juick.number Text]

    $w tag configure JNUM -foreground $options(juick.number)
}

proc configure_juick_ligth {w} {
    set options(juick.private_foreground) \
        [option get $w juick.private_foreground Text]

    set options(juick.private_background) \
        [option get $w juick.private_background Text]

    $w tag configure JLIGTH -foreground $options(juick.private_foreground)
    $w tag configure JLIGTH -background $options(juick.private_background)
}

proc configure_citing {w} {
    set options(juick.citing) [option get $w juick.citing Text]

    $w tag configure CITING -foreground $options(juick.citing)
}

proc spot_juick_ligth {what at startVar endVar} {
    set matched [regexp -indices -start $at -- \
        {(^Private message)(?: from @.+:\n)} $what -> bounds]

    if {!$matched} { return false }

    upvar 1 $startVar uStart $endVar uEnd
    lassign $bounds uStart uEnd
    return true
}

proc spot_citing {what at startVar endVar} {
    set matched [regexp -indices -start $at -- \
        {(?:\n|\A)(>[^\n]+)} $what -> bounds]

    if {!$matched} { return false }

    upvar 1 $startVar uStart $endVar uEnd
    lassign $bounds uStart uEnd
    return true
}

proc spot_juick {what at startVar endVar} {
    set matched [regexp -indices -start $at -- \
        {(?:\s|\n|\A|\(|\>)(@[\w@.-]+|\*[\w?!+'/.-]+)(?:(\.(\s|\n))?)} \
        $what -> bounds]

    if {!$matched} { return false }

    upvar 1 $startVar uStart $endVar uEnd
    lassign $bounds uStart uEnd
    return true
}

proc spot_juick_numbers {what at startVar endVar} {
    set matched [regexp -indices -start $at -- \
        {(?:\s|\n|\A|\(|\>)(#\d+(/\d+)?)(?:(\.(\s|\n))?)} $what -> bounds]

    if {!$matched} { return false }

    upvar 1 $startVar uStart $endVar uEnd
    lassign $bounds uStart uEnd
    return true
}

proc process_juick {atLevel accName} {
    if {[::richtext::property_exists {JUICK}]} {
        return [process $atLevel $accName juick]
    }
}

proc process_juick_numbers {atLevel accName} {
    return [process $atLevel $accName juick_numbers]
}

proc process_citing {atLevel accName} {
    if {[::richtext::property_exists {JUICK}]} {
        return [process $atLevel $accName citing]
    }
}

proc process_juick_ligth {atLevel accName} {
    if {[::richtext::property_exists {JUICK}]} {
       return [process $atLevel $accName juick_ligth]
    }
}

proc process {atLevel accName what} {
    upvar #$atLevel $accName chunks

    set out {}

    foreach {s type tags} $chunks {
        if {[lsearch -regexp $type (text)|(citing)|(juick_ligth)] < 0} {
            # pass through
            lappend out $s $type $tags
            continue
        }

        if {[lsearch -exact $type citing] >=0} {
            lappend tags CITING
        }

        if {[lsearch -exact $type juick_ligth] >= 0} {
            lappend tags JLIGTH
        }

        set index 0; set uStart 0; set uEnd 0
        while {[eval {spot_$what $s $index uStart uEnd}]} {
            if {$uStart - $index > 0} {
                # Write out text before current thing, if any:
                lappend out \
                    [string range $s $index [expr {$uStart - 1}]] \
                    $type $tags
            }

            set thing [string range $s $uStart $uEnd]
            # Write out current thing:
            lappend out $thing $what $tags
            set index [expr {$uEnd + 1}]
        }
        # Write out text after the last thing, if any:
        if {[string length $s] - $index > 0} {
            lappend out [string range $s $index end] $type $tags
        }
    }
    set chunks $out
}

proc render_juick {w type thing tags args} {
    if {[lsearch -exact $tags CITING] < 0 && \
        [lsearch -exact $tags JLIGTH] < 0} \
    {
        if {[string equal [string index $thing 0] "#"]} {
            set type JNUM
        } elseif {[string equal [string index $thing 0] "*"]} {
            set type JTAG
        } elseif {[string equal [string index $thing 0] "@"]} {
            set type JNICK
        }
    } else {
           if {[lsearch -exact $tags CITING] >= 0} {
               set type CITING
           }
           if {[lsearch -exact $tags JLIGTH] >= 0} {
               set type JLIGTH
           }
    }

#################
            variable chat_things
            set cw [join [lrange [split $w .] 0 end-1] .]
            set chatid [chat::winid_to_chatid $cw]
            if {![info exist chat_things($chatid)]} {
                set chat_things($chatid) [list $thing]
            } else {
                set chat_things($chatid) [linsert \
                    $chat_things($chatid) 0 $thing]
            }
#################

    set id JUICK-$thing
    $w insert end $thing [lfuse $tags [list $id $type JUICK]]
    return $id
}

proc render_citing {w type thing tags args} {
    set id CITING-$thing
    $w insert end $thing [lfuse $tags [list $id $type CITING]]
    return $id
}

proc render_juick_ligth {w type thing tags args} {
    set id JLIGTH-$thing
    $w insert end $thing [lfuse $tags [list $id $type JLIGTH]]
    return $id
}

::richtext::register_entity juick_numbers \
    -configurator [namespace current]::configure_juick_numbers \
    -parser [namespace current]::process_juick_numbers \
    -renderer [namespace current]::render_juick \
    -parser-priority 54

::richtext::register_entity juick_ligth \
    -configurator [namespace current]::configure_juick_ligth \
    -parser [namespace current]::process_juick_ligth \
    -renderer [namespace current]::render_juick_ligth \
    -parser-priority 81

::richtext::register_entity citing \
    -configurator [namespace current]::configure_citing \
    -parser [namespace current]::process_citing \
    -renderer [namespace current]::render_citing \
    -parser-priority 82

::richtext::register_entity juick \
    -configurator [namespace current]::configure_juick \
    -parser [namespace current]::process_juick \
    -renderer [namespace current]::render_juick \
    -parser-priority 85
}
# vi:ts=4:et
