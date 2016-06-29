if {[llength [info procs juick::juick-utils-tcl-sentry]] > 0} {
    return
}

# Just for avoid double including.
proc juick::juick-utils-tcl-sentry {} {}

proc juick::get_clickable_thing_at {w x y} {
    set tags [$w tag names "@$x,$y"]

    if {[lsearch -exact $tags juick_clickable] >= 0} {
        set id [lsearch -glob -inline $tags juick_id_*]
        if {$id == ""} { return "" }
        lassign [$w tag prevrange $id "@$x,$y + 1c"] idx1 idx2
        return [$w get $idx1 $idx2]
    }

    return ""
}

proc juick::is_juick_jid {jid} {
    set jid [::xmpp::jid::removeResource $jid]
    set node [::xmpp::jid::node $jid]
    return [expr {[string equal $jid "juick@juick.com"] || \
        [string equal $node "juick%juick.com"]}]
}

# Determines whether given chatid correspond to Juick
proc juick::is_juick {chatid} {
    set jid [chat::get_jid $chatid]
    return [is_juick_jid $jid]
}

proc juick::is_jubo_jid {jid} {
    set jid [::xmpp::jid::removeResource $jid]
    set node [::xmpp::jid::node $jid]
    return [expr {[string equal $jid "jubo@nologin.ru"] || \
        [string equal $node "jubo%nologin.ru"]}]
}

# Determines whether given chatid correspond to Jubo
proc juick::is_jubo {chatid} {
    set jid [chat::get_jid $chatid]
    return [is_jubo_jid $jid]
}

proc juick::get_my_juick_nickname {jid} {
    variable nicknames

    set uname ""
    set jid [::xmpp::jid::removeResource $jid]

    if {[info exists nicknames($jid)]} {
        set uname $nicknames($jid)
    }

    return $uname
}

proc juick::is_personal_juick_message {from body} {
    variable options

    if {![is_juick_jid $from]} { return 0 }

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

proc juick::send_to_juick {w body} {
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

proc juick::get_juick_thread {w thing} {
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

proc juick::receive_juick_thread {jid res child0} {
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

proc juick::copy_thing {w thing} {
    clipboard clear -displayof $w
    clipboard append -displayof $w $thing
}

proc juick::browse_thing {w thing} {
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
