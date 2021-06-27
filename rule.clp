(defclass position (is-a USER)
    (slot element
        (type SYMBOL)
        (allowed-symbols
            anemo cryo electro geo hydro pyro)
    )
    (slot physic_or_not
        (type SYMBOL)
        (allowed-symbols
            yes no)
    )
    (slot e_skill_base
        (type SYMBOL)
        (allowed-symbols
            HP DEF CRIT ATK)
        (default ATK)
    )
    (slot q_skill_base
        (type SYMBOL)
        (allowed-symbols
            HP DEF CRIT ATK)
        (default ATK)
    )
    (multislot e_skill_type
        (type SYMBOL)
        (allowed-symbols
            DMG HEAL SHIELD BONUS NONE)
    )
    (multislot q_skill_type
        (type SYMBOL)
        (allowed-symbols
            DMG HEAL SHIELD BONUS NONE)
    )
    (slot weapon
        (type SYMBOL)
        (allowed-symbols
            sword bow catayst claymore polearm)
    )
    (slot e_cold
        (type INTEGER)
    )
    (slot q_skill_energy
        (type INTEGER)
    )
)
(defclass carry (is-a position)
    (slot frequent_swiri (default no))
)
(defclass seccarry (is-a position)
    (slot in_swiri_team (default no))
    (slot same_element_carry (default no))
)
(defclass support (is-a position)
    (slot in_swiri_team (default no))
    (slot same_element_carry (default no))
)
(defmessage-handler position put-q_skill_type before(?x ?y)
    (printout t ?x " " ?y crlf)
    (if (eq ?x BONUS)
        then
        (assert (qBONUS))
    )
    (if (eq ?x DMG)
        then
        (assert (qDMG))
    )
    (if (eq ?y HEAL)
        then
        (assert (qHEAL))
    )
)
(defmessage-handler position put-q_skill_energy before(?x)
    (printout t ?x crlf)
    (assert(q_energy ?x))
)
(defmessage-handler position put-e_cold before(?x)
    (printout t ?x crlf)
    (assert(e_cold ?x))
)
(defmessage-handler position put-q_skill_base before(?x)
    (printout t ?x crlf)
    (if (eq ?x ATK)
        then
        (assert (qATK))
        (assert(q_skill_base ATK))
    )
)
(defmessage-handler position put-element before(?x)
    (printout t ?x crlf)
    (if (eq ?x pyro)
        then
        (assert(pyro))
    )
)
(defmessage-handler support put-same_element_carry before(?x)
    (printout t ?x crlf)
    (if (eq ?x yes)
        then
        (assert(same_element_carry))
    )
)
(defmessage-handler position put-e_skill_base before(?x)
    (printout t ?x crlf)
    (if (eq ?x ATK)
        then
        (assert(e_skill_base ATK))
        (assert(eATK))
    )
    (if (eq ?x HP)
        then
        (assert(e_skill_base HP))
        (assert(eHP))
    )
)
(defrule recharge
    (e_cold ?x)
    (q_energy ?y)
    =>
    (assert(e_cold_inf (/ ?y 60)))
    (assert (q_skill_energy_inf (/ ?x 4)))
)
(defrule recharge2
    (e_cold_inf ?x)
    (q_skill_energy_inf ?y)
    =>
    (assert(recharge (+ ?x ?y)))
)
(defrule SandNotC1
    (e_skill_base ?x)
    (q_skill_base ?y)
    (recharge ?z)
    (support)
    =>
    (if (> ?z 2)
        then
        (assert(Sands recharge))
     else
        (assert(Sands ?x))
    )
)
(defrule SandNotC2
    (e_skill_base ?x)
    (q_skill_base ?y)
    (recharge ?z)
    (seccarry)
    =>
    (if (> ?z 2)
        then
        (assert(Sands recharge))
     else
        (assert(Sands ?x))
    )
)
(defrule SandC
    (e_skill_base ?x)
    (q_skill_base ?y)
    (recharge ?z)
    (carry)
    =>
    (assert(Sands ?x))
)
(defrule Noblesse1
    (qBONUS)
    (qATK)
    =>
    (assert(Noblesse))
)
(defrule Noblesse2
    (qDMG)
    (qATK)
    =>
    (assert(Noblesse))
)
(defrule Crimson_witch
    (qDMG)
    (pyro)
    =>
    (assert(Crimson_witch))
)
(defrule pyroCarryOut
    (Crimson_witch)
    (carry)
    =>
    (assert(FourCrimson_witch))
)
(defrule pyroSecCarryOut
    (Crimson_witch)
    (seccarry)
    (Noblesse)
    =>
    (assert(Crimson_witchPlusNoblesse))
)
(defrule pyroSecCarryOut2
    (same_element_carry)
    (seccarry)
    (pyro)
    =>
    (assert(Lavawalker))
)
(defrule pyroCrimsonSupOut
    (Crimson_witch)
    (seccarry)
    (Noblesse)
    =>
    (assert(FourNoblesse))
    (assert(Crimson_witchPlusNoblesse))
)
(defrule pyroNoCrimsonSupOut
    (support)
    (Noblesse)
    =>
    (assert(FourNoblesse))
)