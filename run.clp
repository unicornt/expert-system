(make-instance [bennet] of
    support
        (element pyro)
        (physic_or_not no)
        (e_skill_base ATK)
        (q_skill_base ATK)
        (e_skill_type DMG)
        (e_cold 5)
        (q_skill_type BONUS HEAL)
        (weapon sword)
        (q_skill_energy 60)
        (same_element_carry no)
        (in_swiri_team no)
)
(make-instance [xiangling] of
    seccarry
        (element pyro)
        (physic_or_not no)
        (e_skill_base ATK)
        (q_skill_base ATK)
        (e_skill_type DMG)
        (e_cold 12)
        (q_skill_type DMG NONE)
        (weapon polearm)
        (q_skill_energy 80)
        (same_element_carry no)
        (in_swiri_team no)
)
(make-instance [hutao] of
    carry
        (element pyro)
        (physic_or_not no)
        (e_skill_base HP)
        (q_skill_base ATK)
        (e_skill_type DMG)
        (e_cold 16)
        (q_skill_type DMG NONE)
        (weapon polearm)
        (q_skill_energy 60)
)