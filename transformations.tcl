%s/\${::#EOF}/[::EndOfFile new]/gc
%s/#EOF/[::EndOfFile new]/gc
%s/\${::#t}/[::True new]/gc
%s/\${::#f}/[::False new]/gc
%s/\${::#NIL}/[::EmptyList new]/gc
%s/\${::#UND}/[::Undefined new]/gc
%s/\${::#UNS}/[::Unspecified new]/gc
