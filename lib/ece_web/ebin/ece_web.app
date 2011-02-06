%% This is the application resource file (.app file) for the ece_web,
%% application.
{application, ece_web,
  [{description, "Your Desc HERE"},
   {vsn, "0.1.0"},
   {modules, [ece_web_app,
              ece_web_sup]},
   {registered,[ece_web_sup]},
   {applications, [kernel, stdlib]},
   {mod, {ece_web_app,[]}},
   {start_phases, []}]}.

