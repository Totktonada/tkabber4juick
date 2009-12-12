package require msgcat

namespace eval juick {

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

proc load {} {
    ::richtext::entity_state citing 1
    ::richtext::entity_state juick 1

    hook::add draw_message_hook [namespace current]::ignore_server_messages 0
    hook::add draw_message_hook [namespace current]::handle_message 21
    hook::add chat_window_click_hook [namespace current]::insert_from_window
    hook::add chat_win_popup_menu_hook [namespace current]::add_juick_things_menu 20
}

proc unload {} {
    hook::remove draw_message_hook [namespace current]::ignore_server_messages 0
    hook::remove draw_message_hook [namespace current]::handle_message 21
    hook::remove chat_window_click_hook [namespace current]::insert_from_window
    hook::remove chat_win_popup_menu_hook [namespace current]::add_juick_things_menu 20

    ::richtext::entity_state citing 0
    ::richtext::entity_state juick 0
}

# Determines whether given chatid correspond to Juick
proc is_juick {chatid} {
    set jid [chat::get_jid $chatid]
#    set jid [chat::get_jid [chat::winid_to_chatid [join [lrange [split $w .] 0 end-1] .]]]
    return [expr [cequal $jid "juick@juick.com/Juick"] || [regexp "juick%juick.com@.+/Juick" $jid]]
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

proc ignore_server_messages {chatid from type body x} {
    if {[is_juick $chatid] && $from == ""} {
        return stop;
    }
}

proc insert_from_window {chatid w x y} {
    set thing ""
    set cw [chat::chat_win $chatid]
    set ci [chat::input_win $chatid]
    set tags [$cw tag names "@$x,$y"]

    if {[set idx [lsearch -glob $tags JUICK-*]] >= 0} {
        set thing [string range [lindex $tags $idx] 6 end]
    }

    if {$thing == ""} return

    $ci insert insert "$thing "
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
}

proc copy_thing {w thing} {
    clipboard clear -displayof $w
    clipboard append -displayof $w $thing
}

variable commands {HELP NICK LOGIN S U ON OFF D BL WL PM VCARD PING INVITE}
proc correct_command {chatid user body type} {
   # Maybe once I'll get arount to it 
}

# --------------
# RichText stuff

proc configure_juick {w} {
    $w tag configure JNICK -foreground red
    $w tag configure JTAG -foreground ForestGreen
    $w tag configure JNUM -foreground blue
    $w tag configure JMY -foreground gray
}

proc configure_citing {w} {
    $w tag configure CITING -foreground gray35
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
    {(?:\s|\n|\A|\(|\>)(#\d+(/\d+)?|@[\w@.-]+|\*[\w?!+'/.-]+)(?:(\.(\s|\n))?)} $what -> bounds]

    if {!$matched} { return false }

    upvar 1 $startVar uStart $endVar uEnd
    lassign $bounds uStart uEnd
    return true
}

proc process_juick {atLevel accName} {
return [process $atLevel $accName juick]
}

proc process_citing {atLevel accName} {
return [process $atLevel $accName citing]
}

proc process {atLevel accName what} {
    upvar #$atLevel $accName chunks

    if {![::richtext::property_exists {JUICK}]} {return}
    set out {}

    foreach {s type tags} $chunks {
        if {[lsearch -regexp $type (text)|(citing)]<0} {
            # pass through
            lappend out $s $type $tags
            continue
        }

        if {[expr [lsearch -exact $type citing]>=0]} {
        lappend tags CITING
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
    if {[lsearch -exact $tags CITING]<0} {
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
           set type CITING
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
