%% This is the application resource file (.app file) for the ece_db,
%% application.
{application, ece_db,
  [{description, "eCloudEdit database interface"},
   {vsn, "0.0.1"},
   {modules, [ece_db_app,
              ece_db_sup]},
   {registered,[ece_db_sup]},
   {applications, [kernel, stdlib, couchbeam]},
   {mod, {ece_db_app,[]}},
   {start_phases, []}]}.

