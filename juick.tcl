namespace eval juick {
proc render_body {chatw mes} {
    while {![cequal $mes ""]} {
	set loc ""
	regexp -indices {(#\d+|@\S+)} $mes loc
	if {[cequal $loc ""]} { $chatw insert end $mes; break } else {
	    set ms [lindex $loc 0]
	    set me [lindex $loc 1]
	    $chatw insert end [string range $mes 0 [expr {$ms - 1}]]
	    if { [cequal [string index $mes $ms] "#" ]} {set tag jnum} else {set tag jnick}
	    $chatw insert end [string range $mes $ms $me] $tag
	    set mes [string range $mes [expr {$me+1}] end]
	}
    }
}

proc handle_message {chatid from type body x} {
    if {[cequal $from "juick@juick.com/Juick"]} {
        set chatw [chat::chat_win $chatid]
	$chatw tag configure jnick -foreground red
	$chatw tag configure jnum -foreground blue
        #$chatw insert end $body
	render_body $chatw $body
	return stop
    }
}

hook::add draw_message_hook [namespace current]::handle_message 10
}
