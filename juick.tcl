package require msgcat

option add *juick.nick			red		widgetDefault
option add *juick.tag			ForestGreen	widgetDefault
option add *juick.my			gray		widgetDefault
option add *juick.number		blue		widgetDefault
option add *juick.private_foreground	blue		widgetDefault
option add *juick.private_background	#FF9A15		widgetDefault
option add *juick.citing		gray35		widgetDefault

namespace eval juick {
variable options
::msgcat::mcload [file join [file dirname [info script]] msgs]

if {![::plugins::is_registered juick]} {
    ::plugins::register juick \
              -namespace [namespace current] \
              -source [info script] \
              -description [::msgcat::mc "Whether the Juick plugin is loaded."] \
              -loadcommand [namespace code load] \
              -unloadcommand [namespace code unload]
    return
    }

        custom::defgroup Plugins [::msgcat::mc "Plugins options."] -group Tkabber

        set group "Juick"

        custom::defgroup $group \
                [::msgcat::mc "Juick settings."] \
                -group Plugins
        custom::defvar options(juick_jids) "juick@juick.com/Juick" \
                [::msgcat::mc "List of JIDs we treated as Juick JIDs."] \
                -group $group \
                -type string
        custom::defvar options(main_jid) "juick@juick.com/Juick" \
                [::msgcat::mc "Main Juick JID used for forwarding things from supplementary bots."] \
                -group $group \
                -type string
        custom::defvar options(juick_bots) "jubo@nologin.ru/jubo" \
                [::msgcat::mc "Forward things from this list of JIDs to Main Juick JID."] \
                -group $group \
                -type string
        custom::defvar options(special_update_juick_tab) 1 \
                [::msgcat::mc "Only PM and reply to your comments is personal message."] \
                -group $group \
                -type boolean

proc load {} {
    ::richtext::entity_state juick_numbers 1
    ::richtext::entity_state citing 1
    ::richtext::entity_state juick 1
    ::richtext::entity_state juick_ligth 1

    hook::add draw_message_hook        [namespace current]::ignore_server_messages 0
    hook::add draw_message_hook        [namespace current]::handle_message 21
    hook::add chat_window_click_hook   [namespace current]::insert_from_window
    hook::add chat_win_popup_menu_hook [namespace current]::add_juick_things_menu 20
    hook::add rewrite_message_hook     [namespace current]::rewrite_juick_message 20
    hook::add chat_send_message_hook   [namespace current]::rewrite_send_juick_message 19

    hook::add draw_message_hook [namespace current]::update_juick_tab 8
    hook::remove draw_message_hook ::plugins::update_tab::update 8

    hook::add draw_message_hook [namespace current]::add_number_of_messages_from_juick_to_title 18
    hook::remove draw_message_hook ::::ifacetk::add_number_of_messages_to_title 18
}

proc unload {} {
    hook::remove draw_message_hook        [namespace current]::ignore_server_messages 0
    hook::remove draw_message_hook        [namespace current]::handle_message 21
    hook::remove chat_window_click_hook   [namespace current]::insert_from_window
    hook::remove chat_win_popup_menu_hook [namespace current]::add_juick_things_menu 20
    hook::remove rewrite_message_hook     [namespace current]::rewrite_juick_message 20
    hook::remove chat_send_message_hook   [namespace current]::rewrite_send_juick_message 19

    hook::remove draw_message_hook [namespace current]::update_juick_tab 8
    hook::add draw_message_hook ::plugins::update_tab::update 8

    hook::remove draw_message_hook [namespace current]::add_number_of_messages_from_juick_to_title 18
    hook::add draw_message_hook ::::ifacetk::add_number_of_messages_to_title 18

    ::richtext::entity_state juick_numbers 0
    ::richtext::entity_state citing 0
    ::richtext::entity_state juick 0
    ::richtext::entity_state juick_ligth 0
}

proc is_juick_jid {jid} {
    variable options
    set accept_list [split $options(juick_jids) " "]
    return [expr [lsearch -exact $accept_list $jid] >= 0]
}

# Determines whether given chatid correspond to Juick
proc is_juick {chatid} {
    set jid [chat::get_jid $chatid]
    return [is_juick_jid $jid]
#    return [expr [cequal $jid "juick@juick.com/Juick"] || [regexp "juick%juick.com@.+/Juick" $jid]]
}

proc handle_message {chatid from type body x} {
    if {[is_juick $chatid]} {
        ::richtext::property_add {JUICK} {}
        set chatw [chat::chat_win $chatid]
        set jid [chat::get_jid $chatid]

        set tags {}
        if {![cequal $jid $from]} {
            lappend tags JMY
        }

        ::richtext::render_message $chatw $body $tags
        return stop
    }
}

proc update_juick_tab {chatid from type body x} {
    variable options
    if {![expr [is_juick $chatid] && [cequal $type "chat"] && $options(special_update_juick_tab)]} {
        ::plugins::update_tab::update $chatid $from $type $body $x
        return
    }

    # See ${PATH_TO_TKABBER}/plugins/chat/update_tab.tcl
    foreach xelem $x {
        ::xmpp::xml::split $xelem tag xmlns attrs cdata subels
        if {[string equal $tag ""] && [string equal $xmlns tkabber:x:nolog]} {
            return
        }
    }

    set cw [chat::winid $chatid]

    if {[regexp {^Private message from @.+:\n} $body]} {
        tab_set_updated $cw 1 mesg_to_user
    } else {
        tab_set_updated $cw 1 message
    }
}

proc ignore_server_messages {chatid from type body x} {
    if {[is_juick $chatid] && $from == ""} {
        return stop;
    }
}

proc add_number_of_messages_from_juick_to_title {chatid from type body x} {
    variable options
    if {![expr [is_juick $chatid] && [cequal $type "chat"] && $options(special_update_juick_tab)]} {
        ::ifacetk::add_number_of_messages_to_title $chatid $from $type $body $x
        return
    }

    # See ${PATH_TO_TKABBER}/ifacetk/iface.tcl
    foreach xelem $x {
        ::xmpp::xml::split $xelem tag xmlns attrs cdata subels
        if {[string equal $tag ""] && [string equal $xmlns tkabber:x:nolog]} {
            return
        }
    }

    if {[::ifacetk::chat_window_is_active $chatid]} return
    if {$from == ""} return

    variable ::ifacetk::number_msg
    variable ::ifacetk::personal_msg

    incr number_msg($chatid)

    if {[regexp {^Private message from @.+:\n} $body]} {
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

    if {![is_juick_jid $from] || ![cequal $type "chat"]} {
        return
    }

#############################
# Remove jabber:x:oob element
    set newx {}

    foreach xe $x {
        ::xmpp::xml::split $xe tag xmlns attrs cdata subels

        if {![cequal $xmlns "jabber:x:oob"]} {
            lappend newx $xe
        }
    }

    set x $newx

#############################
# Add GMT time
    foreach xe $x {
        ::xmpp::xml::split $xe tag xmlns attrs cdata subels

        if {[cequal $xmlns "http://juick.com/message"]} {
            foreach {key val} $attrs {
                if {[cequal $key "ts"]} {
                    set body "$val GMT\n$body"
                }
            }
        }
    }
}

proc rewrite_send_juick_message {chatid user body type} {
    if {![is_juick $chatid] || ![cequal $type "chat"]} {
        return
    }

#    if {[regexp {^S (#\d+)\+\s*$} $body -> thing]} { }
    if {[regexp {^S (#\S+)\+\s*$} $body -> thing]} {
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
#    set ci [chat::input_win $chatid]

    set jid [chat::get_jid $chatid]
    set forward_list [split $options(juick_bots) " "]
    if {[lsearch -exact $forward_list $jid] >= 0} {
        set mainchat [list [chat::get_xlib $chatid] $options(main_jid)]
        chat::activate $mainchat
        set ci [chat::input_win $mainchat]
    } else {
        set ci [chat::input_win $chatid]
    }
    set tags [$cw tag names "@$x,$y"]

    if {[set idx [lsearch -glob $tags JUICK-*]] >= 0} {
        set thing [string range [lindex $tags $idx] 6 end]
    }

    if {$thing == ""} return

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

    if {$thing == ""} return
    $m add command -label [::msgcat::mc "\[J\] Copy thing to clipboard."] \
          -command [list [namespace current]::copy_thing $chatwin $thing]
    $m add command -label [::msgcat::mc "\[J\] Open thing in browser."] \
          -command [list [namespace current]::browse_thing $chatwin $thing]
    if {[regexp {#\d+$} $thing]} {
        $m add command -label [::msgcat::mc "\[J\] Subscribe to $thing replies."] \
              -command [list [namespace current]::subscribe_to $chatwin $thing]
    }
}

proc subscribe_to {w thing} {
     set cw [join [lrange [split $w .] 0 end-1] .]
     set chatid [chat::winid_to_chatid $cw]
     set xlib [chat::get_xlib $chatid]
     set jid [chat::get_jid $chatid]
     set body "S $thing"

     message::send_msg $xlib $jid -type chat -body $body
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
          browseurl http://juick.com/last?tag=[string range $thing 1 end]
          }
    }
}

variable commands {HELP NICK LOGIN S U ON OFF D BL WL PM VCARD PING INVITE}
proc correct_command {chatid user body type} {
   # Maybe once I'll get arount to it 
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
    set options(juick.private_foreground) [option get $w juick.private_foreground Text]
    set options(juick.private_background) [option get $w juick.private_background Text]

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
    {(?:\s|\n|\A|\(|\>)(@[\w@.-]+|\*[\w?!+'/.-]+)(?:(\.(\s|\n))?)} $what -> bounds]

    if {!$matched} { return false }

    upvar 1 $startVar uStart $endVar uEnd
    lassign $bounds uStart uEnd
    return true
}

proc spot_juick_numbers {what at startVar endVar} {
    set matched [regexp -indices -start $at -- \
    {(?:\s|\n|\A|\(|\>)(#\S+(/\d+)?)(?:(\.(\s|\n))?)} $what -> bounds]
#    {(?:\s|\n|\A|\(|\>)(#\d+(/\d+)?)(?:(\.(\s|\n))?)} $what -> bounds]

    if {!$matched} { return false }

    upvar 1 $startVar uStart $endVar uEnd
    lassign $bounds uStart uEnd
    return true
}

proc process_juick {atLevel accName} {
return [process $atLevel $accName juick]
}

proc process_juick_numbers {atLevel accName} {
return [process $atLevel $accName juick_numbers]
}

proc process_citing {atLevel accName} {
return [process $atLevel $accName citing]
}

proc process_juick_ligth {atLevel accName} {
return [process $atLevel $accName juick_ligth]
}

proc process {atLevel accName what} {
    upvar #$atLevel $accName chunks

    if {![::richtext::property_exists {JUICK}]} {return}
    set out {}

    foreach {s type tags} $chunks {
        if {[lsearch -regexp $type (text)|(citing)|(juick_ligth)]<0} {
            # pass through
            lappend out $s $type $tags
            continue
        }

        if {[expr [lsearch -exact $type citing]>=0]} {
        lappend tags CITING
        }

        if {[expr [lsearch -exact $type juick_ligth]>=0]} {
        lappend tags JLIGTH
        }

        set index 0; set uStart 0; set uEnd 0
        while {[eval {spot_$what $s $index uStart uEnd}]} {
            if {$uStart - $index > 0} {
                # Write out text before current thing, if any:
                lappend out [string range $s $index [expr {$uStart - 1}]] $type $tags
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
    if {[expr [lsearch -exact $tags CITING]<0] && [expr [lsearch -exact $tags JLIGTH]<0]} {
       if {[cequal [string index $thing 0] "#" ]} {
          set type JNUM
          } else {
                 if {[cequal [string index $thing 0] "*" ]} {
                    set type JTAG
                    } else {
                           if {[cequal [string index $thing 0] "@" ]} {
                               set type JNICK
                               }
                           }
                 }
    } else {
           if {[lsearch -exact $tags CITING]>=0} {
               set type CITING
           }
           if {[lsearch -exact $tags JLIGTH]>=0} {
               set type JLIGTH
           }
    }

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
