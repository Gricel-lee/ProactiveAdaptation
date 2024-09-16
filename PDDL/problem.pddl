; Hello World example problem

(define (problem identification_grapes)
(:domain identification)

(:objects
    ; 
    d1 d2 - drone
    l0 l1 l2 l3- location
    
)

(:init
    ; Let's assume that the world can hear us
    ; (not (identified l2))
    ; (not (identified l3))
    ; (position  d1 - l1)
    (position  d1 - l1)
    (drone d1)
    (drone d2)
    (location l1)
    (location l2)
    (location l3)
    
)

(:goal
    (and
        ; The only goal is to reach a state where we said 'hello'
        (identified l2)
        (identified l3)
    )
)
)