; Hello World example domain

(define (domain qdc)

(:requirements :strips :typing :negative-preconditions)

(:types drone location)

(:predicates
    (at ?t - drone ?l - location) ; can the `drone` hear
    (said_hello_to ?t - drone) ; records that was said to the `drone`
    (sampled ?l - location) ; records that the `drone` sampled the `location`
)

; this action greets one drone by its name


(:action move2
    :parameters (?t - drone ?l - location)
    :precondition (and
        ; not sampled
        (not (sampled ?l))
        ; robot not start at it
        ;(at ?t ?l)
    )
    :effect (and
        ; record that we said hello
        ;(said_hello_to ?t)
        (sampled ?l)
    )
)


(:action move3
    :parameters (?t - drone ?l - location)
    :precondition (and
        ; we only ever need to greet once
        (not (said_hello_to ?t))
        ; only greet someone if they are near
        (at ?t ?l)
    )
    :effect (and
        ; record that we said hello
        (said_hello_to ?t)
    )
)


)