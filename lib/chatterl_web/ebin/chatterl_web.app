{application, chatterl_web,
 [{description, "chatterl_web"},
  {vsn, "0.0.1"},
  {modules, [
    chatterl_web,
    chatterl_mid_man,
    chatterl_web_web,	
    chatterl_web_app,
    chatterl_web_sup,
    chatterl_web_deps,
    main_controller
  ]},
  {registered, []},
  {mod, {chatterl_web_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
