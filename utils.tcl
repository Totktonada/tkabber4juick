if {[info exist ::juick-utils-tcl-sentry]} {
    return
}

set ::juick-utils-tcl-sentry 1

# invoked from namespace ::plugins::juick

proc get_tags_list {} {
    variable richtext_tags
    set res {}

    foreach {tag_name _ _} $richtext_tags {
        lappend res $tag_name
    }

    return [lsort -unique $res]
}

proc is_juick_jid {jid} {
    set jid [::xmpp::jid::removeResource $jid]
    set node [::xmpp::jid::node $jid]
    return [expr {[string equal $jid "juick@juick.com"] || \
        [string equal $node "juick%juick.com"]}]
}

# Determines whether given chatid correspond to Juick
proc is_juick {chatid} {
    set jid [chat::get_jid $chatid]
    return [is_juick_jid $jid]
}

proc get_my_juick_nickname {jid} {
    variable nicknames

    set uname ""
    set jid [::xmpp::jid::removeResource $jid]

    if {[info exists nicknames($jid)]} {
        set uname $nicknames($jid)
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
