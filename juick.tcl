namespace eval juick {

# Determines whether given chatid correspond to Juick
proc is_juick {chatid} {
    set jid [chat::get_jid $chatid]
#    return [cequal $jid "juick@juick.com/Juick"]
     return [expr [cequal $jid "juick@juick.com/Juick"] || [regexp "juick%juick.com@.*/Juick" $jid]]
}

proc handle_message {chatid from type body x} {
    if {[is_juick $chatid]} {
        ::richtext::property_add {JUICK} {}
        set chatw [chat::chat_win $chatid]
        set jid [chat::get_jid $chatid]
        $chatw tag configure JNICK -foreground red
        $chatw tag configure JTAG -foreground ForestGreen
        $chatw tag configure JNUM -foreground blue
        $chatw tag configure JMY -foreground gray
        if {[cequal $jid $from]} {
            ::richtext::render_message $chatw $body {}
        } else {
            ::richtext::render_message $chatw $body JMY
        }
        return stop
    }
}

hook::add draw_message_hook [namespace current]::handle_message 10

proc ignore_server_messages {chatid from type body x} {
    if {[is_juick $chatid] && $from == ""} {
        return stop;
    }
}

hook::add draw_message_hook [namespace current]::ignore_server_messages 0

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

hook::add chat_window_click_hook \
    [namespace current]::insert_from_window

variable commands {HELP NICK LOGIN S U ON OFF D BL WL PM VCARD PING}
proc correct_command {chatid user body type} {
   # Maybe once I'll get arount to it 
}

# --------------
# RichText stuff

proc configure_richtext_widget {w} {
    $w tag configure JNICK -foreground red
    $w tag configure JTAG -foreground ForestGreen
    $w tag configure JNUM -foreground blue
    $w tag configure JMY -foreground gray
}

proc spot {what at startVar endVar} {
    set matched [regexp -indices \
        -start $at -- {(?:\s|\n|\A)(#\d+(/\d+)?|@[\w@.-]+|\*[\w?.-]+)} $what -> bounds]

    if {!$matched} { return false }

    upvar 1 $startVar us $endVar ue
    lassign $bounds us ue
    return true
}

proc process {atLevel accName} {
    upvar #$atLevel $accName chunks

    if {![::richtext::property_exists {JUICK}]} {return}

    set out {}

    foreach {s type tags} $chunks {
        if {$type != "text"} {
            # pass through
            lappend out $s $type $tags
            continue
        }

        set ix 0; set us 0; set ue 0
        
        while {[spot $s $ix us ue]} {
            if {$us - $ix > 0} {
                # dump chunk before juick
                lappend out [string range $s $ix [expr {$us - 1}]] $type $tags
            }

            set thing [string range $s $us $ue]

            lappend out $thing juick $tags

            set ix [expr {$ue + 1}]
        }

        if {[string length $s] - $ix > 0} {
        lappend out [string range $s $ix end] $type $tags
        }
    }

    set chunks $out
}

proc render {w type thing tags args} {
    if {[cequal [string index $thing 0] "#" ]} {
        set type JNUM
    } else {
           if {[cequal [string index $thing 0] "*" ]} {
               set type JTAG
           } else {
                  set type JNICK
                  }
           }
    set id JUICK-$thing
    $w insert end $thing [lfuse $tags [list $id $type JUICK]]
    return $id
}

::richtext::register_entity juick \
    -configurator [namespace current]::configure_richtext_widget \
    -parser [namespace current]::process \
    -renderer [namespace current]::render \
    -parser-priority 60

    ::richtext::entity_state juick 1
}
# vi:ts=4:et
