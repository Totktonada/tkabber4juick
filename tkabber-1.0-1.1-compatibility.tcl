if {[info exists ::tkabber-1.0-1.1-compatibility-tcl-sentry]} {
    return
}

# Just for avoid double including.
set ::tkabber-1.0-1.1-compatibility-tcl-sentry 1

proc juick::patch_proc {proc_name replace_from replace_to} {
    set proc_args [info args $proc_name]
    set proc_body [info body $proc_name]

    set fst [string first $replace_from $proc_body]
    set lst [expr {$fst + [string length $replace_from] - 1}]

    if {$fst == -1} {
        return -code error "Cannot patch $proc_name in [info script]"
    }

    # Restore original arguments list with defaults.
    set proc_args_defs {}
    foreach arg $proc_args {
        if {[info default $proc_name $arg def]} {
            lappend proc_args_defs [list $arg $def]
        } else {
            lappend proc_args_defs $arg
        }
    }

    set new_proc_body [string replace $proc_body $fst $lst $replace_to]

    rename $proc_name ""
    eval [format "proc %s {%s} {%s}" $proc_name $proc_args_defs $new_proc_body]
}

juick::patch_proc ::richtext::render_message \
    {$entities($type,parser) [info level] chunks} \
    {eval [linsert $entities($type,parser) end [info level] chunks]}

# Used once, already not needed.
rename juick::patch_proc ""
