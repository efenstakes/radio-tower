{
  application, qnc_radio_tower,
  [
    { vsn, "0.0" }, 
    { description, "a publish subscribe server" },

    { 
      modules, [
                  qnc_radio_tower, qrt_sup, qrt_log, qnc_radio_sup, qnc_radio, qrt_utility 
      ] 
    },

    {
      registered, [
          qnc_radio_tower_sup, qnc_radio
      ]
    },

    {
      applications, [
                       kernel, stdlib
      ]
    },

    { mod, { qnc_radio_tower, [] } }

  ] 
}.