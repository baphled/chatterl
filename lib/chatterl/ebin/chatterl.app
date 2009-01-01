%% This is the application resource file (.app file) for the chatterl,
%% application.
{application, chatterl, 
  [{description, "Erlang based chat system."},
   {vsn, "0.1.0.1"},
   {modules, [chatterl_app, chatterl_groups, chatterl_client, chatterl_serv, chatterl_sup]},
   {registered,[chatterl.groups, chatterl,serv, chatterl_sup]},
   {applications, [kernel, stdlib]},
   {mod, {chatterl_app,[]}},
   {start_phases, []}]}.

