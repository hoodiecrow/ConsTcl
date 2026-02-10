proc makesingleton {NAME TEXT} {
    oo::object create ::$NAME
    oo::objdefine ::$NAME {
        class ::constcl::Base  
        method new {} {return [self]}
        unexport create destroy
    }
    oo::objdefine ::$NAME method tstr {} [list return $TEXT]
}
