%% This is the application resource file (.app file) for the ece_web,
%% application.
{application, ece_web,
  [{description, "Web frontend for eCloudedit"},
   {vsn, "0.1.0"},
   {modules, [ece_web_app,
              ece_web_sup,
              ece_resource_documents,
              ece_resource_static]},
   {registered,[ece_web_sup]},
   {applications, [kernel, stdlib, webmachine]},
   {mod, {ece_web_app,[]}},
   {start_phases, []}]}.

