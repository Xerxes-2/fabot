{
  description = "fabot devshell: dotnet SDK 10 + Node.js 22";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs =
    { nixpkgs, ... }:
    let
      forAllSystems =
        f:
        nixpkgs.lib.genAttrs [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ] (
          system: f nixpkgs.legacyPackages.${system}
        );
    in
    {
      devShells = forAllSystems (pkgs: {
        default =
          let
            dotnet = pkgs.dotnet-sdk_10;
          in
          pkgs.mkShell {
            packages = [
              dotnet
              pkgs.nodejs_22
            ];

            # dotnet tools (fable, fantomas) are restored per-repo via
            # `dotnet tool restore`; keep telemetry off in ephemeral shells.
            DOTNET_CLI_TELEMETRY_OPTOUT = "1";
            DOTNET_NOLOGO = "1";

            # Override any host DOTNET_ROOT (e.g. /usr/share/dotnet) so child
            # processes the SDK spawns (fsc, tools) resolve this SDK's runtime.
            DOTNET_ROOT = "${dotnet}/share/dotnet";
          };
      });
    };
}
