{application, erlrrd,
 [{description, "erlrrd"},
  {vsn, "0.01"},
  {modules, [
    erlrrd_app,
    erlrrd_sup,
    erlrrd
  ]},
  {registered, []},
  {mod, {erlrrd_app, []}},
  {env, []},
  {applications, [kernel, stdlib]}]}.

