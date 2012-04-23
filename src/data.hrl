-record(user, {id :: string(),
               name :: string(),
               email :: string(),
               password :: string(),
               irc :: string()}).

-record(session, {id :: string(),
                  user_id :: string()}).

-record(category, {id :: string(),
                   name :: string(),
                   deleted :: boolean()}).

-record(server, {id :: string(),
                 category_id :: string(),
                 name :: string(),
                 deleted :: boolean()}).

-record(booking, {id :: string(),
                  server_id :: string(),
                  start_date :: integer(),
                  end_date :: integer(),
                  branch :: string(),
                  purpose :: string(),
                  user_id :: string()}).
