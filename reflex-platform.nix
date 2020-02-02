let
  reflex-platform-src = builtins.fetchTarball
    {
      url = "https://github.com/reflex-frp/reflex-platform/archive/1568e673b3b3c3407d5736e9f33b85b502482a7f.tar.gz";
    };
in
import reflex-platform-src {}
