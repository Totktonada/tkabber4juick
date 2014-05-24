if {[info exist ::tkabber-0.11.1-compatibility-sentry]} {
    return
}

set ::tkabber-0.11.1-compatibility-sentry 1

rename ::hook::add ::hook::add_orig

proc ::hook::add {hook func {seq 50}} {
    if {[string equal $hook roster_push_hook] && \
        [string equal $func "::plugins::juick::request_juick_nick"] && \
        $seq == 99} \
    {
        ::hook::add_orig roster_item_hook $func $seq
    } else {
        ::hook::add_orig $hook $func $seq
    }
}

proc ::hook::remove {hook func {seq 50}} {
    variable $hook

    set idx [lsearch -exact [set $hook] [list $func $seq]]
    set $hook [lreplace [set $hook] $idx $idx]
}

proc ::plugins::is_registered {name} {
    variable juick_plugin_loaded

    if {![info exists juick_plugin_loaded]} {
        set juick_plugin_loaded 0
    }

    return $juick_plugin_loaded
}

proc ::plugins::register {name args} {
    variable juick_plugin_loaded

    if {$juick_plugin_loaded} {
        return
    }

    set juick_plugin_loaded 1

    foreach {key val} $args {
        switch -- $key {
            -source {
                source $val
            }
            -loadcommand {
                eval $val
            }
        }
    }
}

proc ::chat::get_xlib {chatid} {
    ::chat::get_connid $chatid
}

namespace eval ::xmpp::jid {}

proc ::xmpp::jid::node {jid} {
    set a [string first @ $jid]
    if {$a < 0} {
        return
    } else {
        set b [string first / $jid]
        if {$b >= 0 && $a > $b} {
            return
        } else {
            string range $jid 0 [incr a -1]
        }
    }
}

proc ::xmpp::jid::server {jid} {
    set a [string first @ $jid]
    set b [string first / $jid]

    if {$a < 0} {
        if {$b < 0} {
            return $jid
        } else {
            string range $jid 0 [incr b -1]
        }
    } else {
        if {$b < 0} {
            string range $jid [incr a] end
        } elseif {$a >= $b} {
            string range $jid 0 [incr b -1]
        } else {
            string range $jid [incr a] [incr b -1]
        }
    }
}

proc ::xmpp::jid::removeResource {jid} {
    set node     [node $jid]
    set server   [server $jid]

    if {[string equal $node ""]} {
        set jid $server
    } else {
        set jid $node@$server
    }

    return $jid
}

namespace eval ::xmpp::xml {}

proc ::xmpp::xml::split {xmldata tagVar xmlnsVar attrsVar \
                        cdataVar subelsVar} {
    upvar 1 $tagVar tag $xmlnsVar xmlns $attrsVar attrs $cdataVar cdata \
            $subelsVar subels

    jlib::wrapper:splitxml $xmldata tag attrs isempty cdata subels

    foreach {key val} $attrs {
        switch -- $key {
            xmlns {
                set xmlns $val
            }
        }
    }
}

proc ::xmpp::xml::create {tag args} {
    set invoke [list jlib::wrapper:createtag $tag -vars]

    foreach {key val} $args {
        switch -- $key {
            -xmlns {
                lappend invoke [list xmlns $val]
            }
            -attrs {
                lappend invoke $val
            }
        }
    }

    eval $invoke
}

proc ::xmpp::sendIQ {xlib type args} {
    set invoke [list jlib::send_iq $type]
    set xmldata {}
    set newargs {}

    foreach {key val} $args {
        switch -- $key {
            -query {
                set xmldata $val
            }
            -to {
                lappend newargs $key $val
            }
            -command {
                lappend newargs $key \
                    [list [namespace current]::sendIQAnswer $val]
            }
        }
    }

    lappend invoke $xmldata
    lappend invoke -connection $xlib

    foreach arg $newargs {
        lappend invoke $arg
    }

    eval $invoke
}

proc ::xmpp::sendIQAnswer {cmd res child} {
    if {[string equal $res OK]} {
        set res ok
    }

    uplevel #0 $cmd [list $res $child]
}

proc ::xmpp::xml::getAttr {attrList attrName {fallback ""}} {
    set res $fallback

    foreach {attr val} $attrList {
        if {[string equal $attr $attrName]} {
            set res $val
        }
    }

    return $res
}

proc ::chat::activate {chatid} {
    raise_win [winid $chatid]
    focus -force [input_win $chatid]
}

rename ::message::send_msg ::message::send_msg_tkabber_0_11_1

proc ::message::send_msg {xlib args} {
    set invoke [list ::message::send_msg_tkabber_0_11_1]

    if {[llength $args] % 2 == 0} {
        # tkabber-0.11.1 invoke
        set jid $xlib
        lappend invoke $jid
    } else {
        set jid [lindex $args 0]
        set args [lrange $args 1 end]
        lappend invoke $jid -connection $xlib
    }

    foreach {key val} $args {
        lappend invoke $key $val
    }

    eval $invoke
}
