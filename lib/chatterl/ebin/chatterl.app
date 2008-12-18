%% This is the application resource file (.app file) for the chatterl,
%% application.
{application, chatterl, 
  [{description, "Your Desc HERE"},
   {vsn, "0.1.0"},
   {modules, [chatterl_app,
              chatterl_sup]},
   {registered,[chatterl_sup]},
   {applications, [kernel, stdlib]},
   {mod, {chatterl_app,[]}},
   {start_phases, []}]}.

