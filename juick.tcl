# TODO: jubo #dddddd
# TODO: j2j, see below

# TODO: idea: ... (end of citate) as button for decollapse citate.

namespace eval juick {

package require msgcat
package require http

variable scriptdir [file dirname [info script]]
::msgcat::mcload [file join $scriptdir msgs]

# Register plugin for further load/unload via GUI.
# Stop loading (by source) file, if plugin currently is not registered.
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

# XRDB options
option add *juick_number      blue         widgetDefault
option add *juick_private_fg  blue         widgetDefault
option add *juick_private_bg  #FF9A15      widgetDefault
option add *juick_citing      gray35       widgetDefault
option add *juick_nick        red          widgetDefault
option add *juick_tag         ForestGreen  widgetDefault
option add *juick_my_msg      gray         widgetDefault

# Global variables and constants
# ==============================

variable options
variable nicknames

# list of {tag_name xrdb_name xrdb_option} sequences
# used in 'configurator' proc
variable richtext_tags {
    juick_number  juick_number     foreground
    juick_private juick_private_fg foreground
    juick_private juick_private_bg background
    juick_citing  juick_citing     foreground
    juick_nick    juick_nick       foreground
    juick_tag     juick_tag        foreground
    juick_my_msg  juick_my_msg     foreground
}

# Tags which not configured (has not special colors),
# just contain some information:
# 1. juick_clickable  -- for juick_(number|nick|tag)
# 2. juick_id_*       -- for the same

variable commands {
    S U D BL WL HELP NICK LOGIN ON OFF VCARD PING INVITE
}

# list of {name priority} sequences
variable richtext_parsers {
    juick_md_url_square_brackets  49
    juick_md_url_round_brackets   49
    juick_number                  54
    juick_private                 48
    juick_citing                  48
    juick_nicks_tags              49
}

# list of {hook proc priority orig_proc} sequences
variable plugin_hooks {
    roster_push_hook          determine_juick_nick       99 ""
    draw_message_hook         ignore_server_messages      0 ""
    draw_message_hook         draw_message_handle        21 ""
    chat_window_click_hook    insert_by_click            99 ""
    chat_win_popup_menu_hook  add_chat_things_menu       20 ""
    rewrite_message_hook      rewrite_incoming_message   20 ""
    chat_send_message_hook    rewrite_subscribe_plus_cmd 19 ""
    generate_completions_hook juick_commands_comps       99 ""
    draw_message_hook         update_juick_tab            8 \
        ::plugins::update_tab::update
    draw_message_hook         add_number_to_tab_title    18 \
        ::::ifacetk::add_number_of_messages_to_title
}

# Compatibility with tkabber-1.0* and some tkabber-1.1*
set scriptname "tkabber-1.0-1.1-compatibility.tcl"
namespace eval [namespace parent] \
    [format {source [file join "%s" "%s"]} $scriptdir $scriptname]

# namespace juick
}

# Procedures
# ==========

proc juick::load {} {
    variable scriptdir
    variable richtext_parsers
    variable plugin_hooks

    namespace eval [namespace parent] \
        [format {source [file join "%s" utils.tcl]} $scriptdir]

    ::richtext::entity_state juick_configurator 1
    ::richtext::register_entity juick_configurator \
        -configurator [namespace current]::configurator

    foreach {name priority} $richtext_parsers {
        ::richtext::entity_state $name 1
        ::richtext::register_entity $name \
            -parser [list [namespace current]::parser $name] \
            -parser-priority $priority
    }

    foreach {hook proc priority orig_proc} $plugin_hooks {
        if {$orig_proc ne ""} {
            hook::remove $hook $orig_proc $priority
        }

        hook::add $hook [namespace current]::$proc $priority
    }

    # Create 'Customize -> Plugins -> Juick' menu
    # Note: we not touch it in 'unload' proc for ability of tuning parameters
    # with unloaded plugin.
    custom::defgroup Plugins [::msgcat::mc "Plugins options."] \
        -group Tkabber
    custom::defgroup Juick [::msgcat::mc "Juick settings."] \
        -group Plugins
    custom::defvar options(main_jid) "juick@juick.com/Juick" \
        [::msgcat::mc "Main Juick JID. This used for\
            forwarding things from other chats."] \
        -group Juick -type string
    custom::defvar options(special_update_juick_tab) 1 \
        [::msgcat::mc "Indicate as personal message only private\
            messages and replies to you.\nYour Juick nickname\
            determines at roster receiving, so after enable option\
            you need to reconnecting."] \
        -group Juick -type boolean
}

proc juick::unload {} {
    variable richtext_parsers
    variable plugin_hooks

    ::richtext::unregister_entity juick_configurator
    ::richtext::entity_state juick_configurator 0

    foreach {name _} $richtext_parsers {
        ::richtext::unregister_entity $name
        ::richtext::entity_state $name 0
    }

    foreach {hook proc priority orig_proc} $plugin_hooks {
        hook::remove $hook [namespace current]::$proc $priority

        if {$orig_proc ne ""} {
            hook::add $hook $orig_proc $priority
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
    if {[is_juick $chatid]} {
        # Allow juick::parser to process given chat as a juick chat.
        ::richtext::property_add {PROCESS_AS_JUICK_MESSAGE} {}
    } elseif {[is_jubo $chatid]} {
        # Allow juick::parser to process given chat as a jubo chat.
        ::richtext::property_add {PROCESS_AS_JUBO_MESSAGE} {}
    } else {
        # Disallow juick::parser to process message.
        return
    }

    set chatw [chat::chat_win $chatid]
    set jid [chat::get_jid $chatid]

    if {$jid ne $from} {
        ::richtext::render_message $chatw $body {juick_my_msg}
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

    set ci [chat::input_win $chatid]
    set jid [::xmpp::jid::removeResource [chat::get_jid $chatid]]
    set thing [get_clickable_thing_at $w $x $y]
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
    set thing [get_clickable_thing_at $chatwin $x $y]
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

proc juick::juick_commands_comps {chatid compsvar wordstart line} {
    upvar 0 $compsvar comps
    variable commands

    if {![is_juick $chatid]} return

    # Collect chat things
    set chat_items {}
    set chatw [chat::chat_win $chatid]
    set tags [$chatw tag names]
    set ids [lsearch -glob -inline -all $tags juick_id_*]
    foreach id $ids {
        foreach {idx1 idx2} [$chatw tag ranges $id] {
            set thing [$chatw get $idx1 $idx2]
            set chat_items [linsert $chat_items 0 $idx1 $thing]
        }
    }

    # Sort chat things
    set chat_things {}
    set chat_items [lsort -real -decreasing -stride 2 -index 0 $chat_items]
    foreach {idx thing} $chat_items {
        lappend chat_things $thing
    }

    set comps [concat $chat_things $commands $comps]
}

# RichText stuff
# --------------

proc juick::configurator {w} {
    variable options
    variable richtext_tags

    # TODO: check window for juick/jubo and exit if not on it.

    foreach {tag_name xrdb_name xrdb_option} $richtext_tags {
        if {$xrdb_name ne ""} {
            set options($xrdb_name) [option get $w $xrdb_name Text]
            $w tag configure $tag_name -$xrdb_option $options($xrdb_name)
        }
    }

    # Display all things and citing in our message with 'my message' colors.
    $w tag raise juick_my_msg juick_citing

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

proc juick::parser_spot_md_url {ptype what at startVar endVar url_infoVar} {
    variable ::plugins::urls::url_regexp
    upvar 1 $url_infoVar url_info

    switch -exact $ptype {
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

    set url_info [list $title $url]

    return true
}

proc juick::parser_spot {ptype processing_type what at startVar endVar \
    url_infoVar} \
{
    upvar 1 $startVar uStart $endVar uEnd
    upvar 1 $url_infoVar url_info

    switch -glob $ptype {
        juick_md_url* {
            return [parser_spot_md_url $ptype $what $at uStart uEnd url_info]
        }
        juick_number {
            set re {(?:\s|\n|\A|\(|\>)(#\d+(/\d+)?)(?:(\.(\s|\n))?)}
        }
        juick_private {
            if {$processing_type != "juick"} { return false }
            set re {(^Private message)(?: from @.+:\n)}
        }
        juick_citing {
            if {$processing_type != "juick"} { return false }
            set re {(?:\n|\A)(>[^\n]+)}
        }
        juick_nicks_tags {
            set re {(?:\s|\n|\A|\(|\>)(@[\w@.-]+|\*[\w?!+'/.-@*]+)(?:(\.(\s|\n))?)}
        }
    }

    set matched [regexp -indices -start $at -- $re $what -> bounds]
    if {!$matched} { return false }
    lassign $bounds uStart uEnd
    set thing [string range $what $uStart $uEnd]

    return true
}

proc juick::update_tags_type {ptype thing id tagsVar typeVar} {
    upvar 1 $tagsVar tags
    upvar 1 $typeVar type

    if {$ptype eq "juick_nicks_tags"} {
        switch -exact [string index $thing 0] {
            "@" { set newtag juick_nick }
            "*" { set newtag juick_tag }
        }
    } else {
        set newtag $ptype
    }

    lappend tags $newtag

    if {[lsearch -exact {juick_number juick_nick juick_tag} $newtag] >= 0} {
        lappend tags juick_clickable
        lappend tags "juick_id_$id"
    }

    set type $newtag
}

proc juick::parser_write {ptype thing id tags url_info outVar} {
    upvar 1 $outVar out
    lassign $url_info title url

    if {[string match juick_md_url* $ptype]} {
        lappend out $url url $tags
        ::richtext::property_update url:title,$url $title
    } else {
        set type {}
        update_tags_type $ptype $thing $id tags type
        lappend out $thing $type $tags
    }
}

proc juick::parser {ptype atLevel accName} {
    upvar #$atLevel $accName chunks

    set processing_type {}
    if {[::richtext::property_exists {PROCESS_AS_JUICK_MESSAGE}]} {
        set processing_type "juick"
    } elseif {[::richtext::property_exists {PROCESS_AS_JUBO_MESSAGE}]} {
        set processing_type "jubo"
    } else {
        return
    }

    set out {}

    foreach {s type tags} $chunks {
        if {[lsearch -exact {text juick_citing} $type] == -1} {
            # pass through
            lappend out $s $type $tags
            continue
        }

        set index 0; set uStart 0; set uEnd 0

        set url_info {}

        while {[parser_spot $ptype $processing_type $s $index uStart uEnd \
            url_info]} \
        {
            # Write out text before current thing, if exists
            if {$uStart - $index > 0} {
                set text_before [string range $s $index [expr {$uStart - 1}]]
                lappend out $text_before $type $tags
            }

            set thing [string range $s $uStart $uEnd]
            set id "${thing}_${uStart}_${uEnd}"
            # Write out current thing
            parser_write $ptype $thing $id $tags $url_info out
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

# vi:ts=4:et
