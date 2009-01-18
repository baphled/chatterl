{application, chatterl_web,
 [{description, "chatterl_web"},
  {vsn, "0.0.1"},
  {modules, [
    chatterl_web,
    chatterl_web_web,
    chatterl_web_sup,
    chatterl_web_deps,
    main_controller
  ]},
  {registered, [chatterl_web]},
  {mod, {chatterl_web, []}},
  {applications, [kernel, stdlib, crypto, mochiweb]}]}.
