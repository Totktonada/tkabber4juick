namespace eval juick {
proc render_body {chatw mes} {
    while {![cequal $mes ""]} {
        set loc ""
        regexp -indices {(#\d+|@[\w@.-]+)} $mes loc
        if {[cequal $loc ""]} { $chatw insert end $mes; break } else {
            set ms [lindex $loc 0]
            set me [lindex $loc 1]
            $chatw insert end [string range $mes 0 [expr {$ms - 1}]]
            set thing [string range $mes $ms $me]
            if { [cequal [string index $mes $ms] "#" ]} {
                set type JNUM
            } else {
                set type JNICK
            }
            set id JUICK-$thing
            $chatw insert end $thing "$id $type"
            set mes [string range $mes [expr {$me+1}] end]
        }
    }
}

proc render_my_message {chatw mes} {
    $chatw insert end $mes "JMY"
}

proc handle_message {chatid from type body x} {
    set jid [chat::get_jid $chatid]
    if {[cequal $jid "juick@juick.com/Juick"]} {
        set chatw [chat::chat_win $chatid]
        $chatw tag configure JNICK -foreground red
        $chatw tag configure JNUM -foreground blue
        $chatw tag configure JMY -foreground gray
        if {[cequal $jid $from]} {
            render_body $chatw $body
        } else {
            render_my_message $chatw $body
        }
        return stop
    }
}

hook::add draw_message_hook [namespace current]::handle_message 10

proc ignore_server_messages {chatid from type body x} {
    if {[string first "juick@juick.com" $chatid] >= 0 && $from == ""} {
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

    $ci insert end "$thing "
    return stop
}

hook::add chat_window_click_hook \
    [namespace current]::insert_from_window

}
# vi:ts=4:et
