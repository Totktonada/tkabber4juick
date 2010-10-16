package require msgcat

option add *juick.nick			red		widgetDefault
option add *juick.tag			ForestGreen	widgetDefault
option add *juick.my			gray		widgetDefault
option add *juick.number		blue		widgetDefault
option add *juick.private_foreground	blue		widgetDefault
option add *juick.private_background	#FF9A15		widgetDefault
option add *juick.citing		gray35		widgetDefault

namespace eval juick {
variable last_message_time 0
variable last_private_time 0

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
    ::richtext::entity_state juick_numbers 1
    ::richtext::entity_state citing 1
    ::richtext::entity_state juick 1
    ::richtext::entity_state juick_ligth 1

    hook::add draw_message_hook [namespace current]::ignore_server_messages 0
    hook::add draw_message_hook [namespace current]::handle_message 21
    hook::add chat_window_click_hook [namespace current]::insert_from_window
    hook::add chat_win_popup_menu_hook [namespace current]::add_juick_things_menu 20
    hook::add chat_send_message_hook [namespace current]::delay_send 11
}

proc unload {} {
    hook::remove draw_message_hook [namespace current]::ignore_server_messages 0
    hook::remove draw_message_hook [namespace current]::handle_message 21
    hook::remove chat_window_click_hook [namespace current]::insert_from_window
    hook::remove chat_win_popup_menu_hook [namespace current]::add_juick_things_menu 20
    hook::remove chat_send_message_hook [namespace current]::delay_send 11

    ::richtext::entity_state juick_numbers 0
    ::richtext::entity_state citing 0
    ::richtext::entity_state juick 0
    ::richtext::entity_state juick_ligth 0
}

# Determines whether given chatid correspond to Juick
proc is_juick {chatid} {
    set jid [chat::get_jid $chatid]
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

proc send_by_timeout {var_last_send_time run_cmd} {
      upvar $var_last_send_time last_send_time

      if {$last_send_time != 0} {
         set wait_time [expr $last_send_time+10001-[clock clicks -milliseconds]]
         if {$wait_time > 0} {
            after $wait_time $run_cmd
            return stop;
         }
      }

      set last_send_time [clock clicks -milliseconds]
      return;
}

proc delay_send {chatid user body type} {
    variable last_message_time
    variable last_private_time

    if {[is_juick $chatid]} {
       set run_cmd [list hook::run chat_send_message_hook $chatid $user $body $type]

       if {[regexp {^#(\d+)(\d+)?[ ]+[^ ]?.*} $body]} {
          return [send_by_timeout last_message_time $run_cmd];
       } elseif {[regexp {^PM @[^ ]+[ ]+[^ ]?.*} $body]} {
          return [send_by_timeout last_private_time $run_cmd];
       }
    }

    return;
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
    {(?:\s|\n|\A|\(|\>)(#\d+(/\d+)?)(?:(\.(\s|\n))?)} $what -> bounds]

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
