; Hello World example domain

(define (domain identification)

(:requirements :adl :typing :negative-preconditions)
(:types drone location - object
)

(:predicates
    (position  ?d - drone ?l - ?location) ; move drone to location
    (identified ?l - location) ; identifies the plant at location
)

; move to location
(:action move
    :parameters (?d - drone, ?l - location)
    :precondition (and
        ; not visited
        (not (identified ?l))
        ; only greet someone if they are near
        (not (position ?d ?l))
    )
    :effect (and
        ; move to new position and identify plant
        (position ?d ?l)
        (identified ?l)
    )
)

)