# This file is generated by composer-plugin-nixify.
# Manual changes might be lost - proceed with caution!

{ lib, php, phpPackages, unzip, stdenv, runCommandLocal, writeText, fetchurl

# Default fetcher.
, fetcher ? (args: fetchurl { inherit (args) name urls sha256; })

}: src:

with lib;

let

  composerPath = phpPackages.composer.src;
  cacheEntries = [
    { name = "clue_stream-filter-1.6.0.0"; filename = "clue/stream-filter/c0c83fa309da9192f88095580c7d149888cacb0d.zip"; sha256 = "5a1416db7d03ffb88efddcb3141360fc506d2dbae2b5f0938e8e3c9540c168b6"; urls = [ "https://nexus.inuits.io/repository/composer/clue/stream-filter/v1.6.0/clue-stream-filter-v1.6.0.zip" ]; }
    { name = "guzzlehttp_promises-1.5.2.0"; filename = "guzzlehttp/promises/172b17427b7e23325ca52a1e9a58736833702a88.zip"; sha256 = "e8efd4c91f3b0302c055cfcbeba9d869a93db8aad2b7f3242e257555fc616aef"; urls = [ "https://nexus.inuits.io/repository/composer/guzzlehttp/promises/1.5.2/guzzlehttp-promises-1.5.2.zip" ]; }
    { name = "guzzlehttp_psr7-2.5.0.0"; filename = "guzzlehttp/psr7/89c0839b31c40e6e739b6aa581de222ec3cebe73.zip"; sha256 = "41fa3379e3d4b5882ba7333cbcbfa37b4ad8d7d1283fe51c8306ac86f280641c"; urls = [ "https://nexus.inuits.io/repository/composer/guzzlehttp/psr7/2.5.0/guzzlehttp-psr7-2.5.0.zip" ]; }
    { name = "http-interop_http-factory-guzzle-1.2.0.0"; filename = "http-interop/http-factory-guzzle/33b6ce941ec6d28a5d903a431e91536c247904dc.zip"; sha256 = "3141024e30f873361921f6bfd543805a6e9c16cefa98a0027ed6a892ec53de5e"; urls = [ "https://nexus.inuits.io/repository/composer/http-interop/http-factory-guzzle/1.2.0/http-interop-http-factory-guzzle-1.2.0.zip" ]; }
    { name = "jean85_pretty-package-versions-2.0.5.0"; filename = "jean85/pretty-package-versions/47529ccbd43c511483f6866fe018e90afbfa6c1b.zip"; sha256 = "089b59065b3b099ba330a322887bc63a1b4188929f9d5ab2af6f17700e85471f"; urls = [ "https://nexus.inuits.io/repository/composer/jean85/pretty-package-versions/2.0.5/jean85-pretty-package-versions-2.0.5.zip" ]; }
    { name = "php-http_client-common-2.6.1.0"; filename = "php-http/client-common/137047c3b3b58e705d7b8e692a1c4e950031a2b1.zip"; sha256 = "cc4907efc38ac390a44b6cc70439219e67a6479dc911e8308b539c187ae45c9d"; urls = [ "https://nexus.inuits.io/repository/composer/php-http/client-common/2.6.1/php-http-client-common-2.6.1.zip" ]; }
    { name = "php-http_discovery-1.17.0.0"; filename = "php-http/discovery/fc1efe8251314f7cd039099068f187f8b4491e3b.zip"; sha256 = "d4b9de3d25a4dae672aed8aa851185a45dc4950aafa1c73e2b817f52cc0711a8"; urls = [ "https://nexus.inuits.io/repository/composer/php-http/discovery/1.17.0/php-http-discovery-1.17.0.zip" ]; }
    { name = "php-http_httplug-2.4.0.0"; filename = "php-http/httplug/663dc9e9e953c160458d6637aff659cc1ac75c71.zip"; sha256 = "80d54f34459b3945bd22bcd2966a5fb98c74306f95e8b07a6c88eecb752885b5"; urls = [ "https://nexus.inuits.io/repository/composer/php-http/httplug/2.4.0/php-http-httplug-2.4.0.zip" ]; }
    { name = "php-http_message-1.14.0.0"; filename = "php-http/message/718765928106327676727662df6b19f86fe10d33.zip"; sha256 = "a1048264540e66da90aae491bcf1262811c5aa61d089cf38b971f9a9589f61c8"; urls = [ "https://nexus.inuits.io/repository/composer/php-http/message/1.14.0/php-http-message-1.14.0.zip" ]; }
    { name = "php-http_message-factory-1.1.0.0"; filename = "php-http/message-factory/b81027e9fcb5ed27261b24491fbd00e2a7088fce.zip"; sha256 = "2bb2bff915b862ebcb5ceb00b8864059d937e60709e2c790dd15fdb4657c0e0d"; urls = [ "https://nexus.inuits.io/repository/composer/php-http/message-factory/1.1.0/php-http-message-factory-1.1.0.zip" ]; }
    { name = "php-http_promise-1.1.0.0"; filename = "php-http/promise/ea829fb4dc3ea30a40f2175275a89b7e4c94905d.zip"; sha256 = "7ff1fcd02530d3c97a2a38dd505d53469cb4d87075e59d52c952d36efacd5776"; urls = [ "https://nexus.inuits.io/repository/composer/php-http/promise/1.1.0/php-http-promise-1.1.0.zip" ]; }
    { name = "psr_container-2.0.2.0"; filename = "psr/container/4095eecc03723032d35014f9566fa81f52560209.zip"; sha256 = "3111f5b69d3a0f690f5f92d7aeacfb247f5ebd3c175088cce4b098e5d1b06ad7"; urls = [ "https://nexus.inuits.io/repository/composer/psr/container/2.0.2/psr-container-2.0.2.zip" ]; }
    { name = "psr_http-client-1.0.2.0"; filename = "psr/http-client/4911f9f9596640cee26394859764ee3fa31261f4.zip"; sha256 = "d56c0b59e3d5873b02ed7345194d9d4687c90c2187d4ff824134f2eb28382927"; urls = [ "https://nexus.inuits.io/repository/composer/psr/http-client/1.0.2/psr-http-client-1.0.2.zip" ]; }
    { name = "psr_http-factory-1.0.2.0"; filename = "psr/http-factory/edb3eb3555091fdf5c2cb3a584f0aa783a41c1e3.zip"; sha256 = "a8cff2adbd84c58c0fe02ed401026c7364c893670352d025f88e060087adf1ef"; urls = [ "https://nexus.inuits.io/repository/composer/psr/http-factory/1.0.2/psr-http-factory-1.0.2.zip" ]; }
    { name = "psr_http-message-2.0.0.0"; filename = "psr/http-message/013b89f08331cf91d87823016645317c922e3291.zip"; sha256 = "d561d2c36e2e3c0ee8b54e0e9652b3bfa13cd763e5e6f93400969001f3a7968d"; urls = [ "https://nexus.inuits.io/repository/composer/psr/http-message/2.0/psr-http-message-2.0.zip" ]; }
    { name = "psr_log-3.0.0.0"; filename = "psr/log/cc2cad1ed32229afd35915d46e567561a496398c.zip"; sha256 = "bbc23516c85615f2f85b707bc10ba26e62033e2afd696e075575b68dc6e31928"; urls = [ "https://nexus.inuits.io/repository/composer/psr/log/3.0.0/psr-log-3.0.0.zip" ]; }
    { name = "ralouphie_getallheaders-3.0.3.0"; filename = "ralouphie/getallheaders/4a65b2990ec23d741c36b09d6831930316d62fd4.zip"; sha256 = "ef0165da5690da666ab4cb88bb4cec0d50372e3e7c922532c5795a3567b367af"; urls = [ "https://nexus.inuits.io/repository/composer/ralouphie/getallheaders/3.0.3/ralouphie-getallheaders-3.0.3.zip" ]; }
    { name = "sentry_sdk-3.3.0.0"; filename = "sentry/sdk/34ccf09534127be2f366ca636611e40be04c81ad.zip"; sha256 = "fb534a4f6a4d1e63bf436943cb1fb2a31c4ccfd2f957d29d7b95cf7884cfa256"; urls = [ "https://nexus.inuits.io/repository/composer/sentry/sdk/3.3.0/sentry-sdk-3.3.0.zip" ]; }
    { name = "sentry_sentry-3.17.0.0"; filename = "sentry/sentry/7935abd50004c8e8602df76eb95e4e1cffaec9e6.zip"; sha256 = "2516b1fef428ef352153fcd01a994f5ada303d66dc7ff52b0ec59848425b7f51"; urls = [ "https://nexus.inuits.io/repository/composer/sentry/sentry/3.17.0/sentry-sentry-3.17.0.zip" ]; }
    { name = "stephank_composer-plugin-nixify-1.1.0.0"; filename = "stephank/composer-plugin-nixify/8aa5304e8b9289c8b16a1de5d26e26d753cd5ada.zip"; sha256 = "ac7cc480698f8717fb9fce4077b81303d37fe6ab2b89c8547cd7f9451598ee1e"; urls = [ "https://nexus.inuits.io/repository/composer/stephank/composer-plugin-nixify/v1.1.0/stephank-composer-plugin-nixify-v1.1.0.zip" ]; }
    { name = "symfony_deprecation-contracts-3.2.1.0"; filename = "symfony/deprecation-contracts/ee84be3744461c2226d2ceb125af34d257abe7f5.zip"; sha256 = "b0b3aa7babdc42b7d8626ac4d329e078e552df0380cda0b7205915ca7c9beafc"; urls = [ "https://nexus.inuits.io/repository/composer/symfony/deprecation-contracts/v3.2.1/symfony-deprecation-contracts-v3.2.1.zip" ]; }
    { name = "symfony_http-client-6.2.10.0"; filename = "symfony/http-client/fadd9aa68b7e6061a6f68fb6103cc91197ef1004.zip"; sha256 = "e135d895b3f753f155c79609b6d0117d6afcffd21dc7f5e7cdf5a79775ae5888"; urls = [ "https://nexus.inuits.io/repository/composer/symfony/http-client/v6.2.10/symfony-http-client-v6.2.10.zip" ]; }
    { name = "symfony_http-client-contracts-3.2.1.0"; filename = "symfony/http-client-contracts/c315b5b0a81cace7f5606fc40c7cee19a5297fd4.zip"; sha256 = "3582bb63f0c53e86f48393b5e7ad9c31f46c84433dd99ccbe16a7ddddd9f0132"; urls = [ "https://nexus.inuits.io/repository/composer/symfony/http-client-contracts/v3.2.1/symfony-http-client-contracts-v3.2.1.zip" ]; }
    { name = "symfony_options-resolver-6.2.7.0"; filename = "symfony/options-resolver/063a134955a3ae38aaafef96a4602ad1669b708d.zip"; sha256 = "029db478e4e5f166eb1978570e4cdf94e93ba8e88d4da382f1728c99f8619566"; urls = [ "https://nexus.inuits.io/repository/composer/symfony/options-resolver/v6.2.7/symfony-options-resolver-v6.2.7.zip" ]; }
    { name = "symfony_polyfill-ctype-1.27.0.0"; filename = "symfony/polyfill-ctype/d4aad3683969c51bfacacd87d5971a4b3b4ba7c4.zip"; sha256 = "66f212bc73088271dbbec1f3b1c3ba21171e4c5ab38676c4d151441f3e38db6f"; urls = [ "https://nexus.inuits.io/repository/composer/symfony/polyfill-ctype/v1.27.0/symfony-polyfill-ctype-v1.27.0.zip" ]; }
    { name = "symfony_polyfill-mbstring-1.27.0.0"; filename = "symfony/polyfill-mbstring/d0d59636e2372a3404f007d16ca74b17e919ab3f.zip"; sha256 = "b112101451a1f76a0cb4edd6595a5733059fd2f7a4cd4d36052526602e26a8fa"; urls = [ "https://nexus.inuits.io/repository/composer/symfony/polyfill-mbstring/v1.27.0/symfony-polyfill-mbstring-v1.27.0.zip" ]; }
    { name = "symfony_polyfill-php80-1.27.0.0"; filename = "symfony/polyfill-php80/15a74c02093d9c03e0e32c62ac589ee363f961a2.zip"; sha256 = "8db38ad9c85c94ad664b89ff9ec9dd5b232dc922f6107bb356d44e9dcf6cde9b"; urls = [ "https://nexus.inuits.io/repository/composer/symfony/polyfill-php80/v1.27.0/symfony-polyfill-php80-v1.27.0.zip" ]; }
    { name = "symfony_service-contracts-3.2.1.0"; filename = "symfony/service-contracts/5cdefab69d1034ce8f702544a65f8636e69279dc.zip"; sha256 = "a368aa78fe67f0a13ec04de1e88c1984dac876eb8d07622e4a0a6d50f7ad643e"; urls = [ "https://nexus.inuits.io/repository/composer/symfony/service-contracts/v3.2.1/symfony-service-contracts-v3.2.1.zip" ]; }
    { name = "twig_twig-3.5.1.0"; filename = "twig/twig/b7a3f50c8a53320f5bbcbefe9d84efefa4eb202a.zip"; sha256 = "2b51937cf768b2e505aa438e0ca487344e1c310550568839ec6e5bae56852b20"; urls = [ "https://nexus.inuits.io/repository/composer/twig/twig/v3.5.1/twig-twig-v3.5.1.zip" ]; }
    { name = "composer_package-versions-deprecated-1.11.99.5"; filename = "composer/package-versions-deprecated/6963288154c9068c272af440012fd9dfb3bbffd3.zip"; sha256 = "557187e1f72faff2b2e61fedd93824c74b491f6078c8135fe2978488e6ee32c3"; urls = [ "https://nexus.inuits.io/repository/composer/composer/package-versions-deprecated/1.11.99.5/composer-package-versions-deprecated-1.11.99.5.zip" ]; }
    { name = "sserbin_twig-linter-3.1.0.0"; filename = "sserbin/twig-linter/91411923070e4236a11c3650b1ed0c183602ffe3.zip"; sha256 = "a86cf19f13c0beb20426b1d8b4eeafc6d551fba964cb06ef63c2c883a1a545e7"; urls = [ "https://nexus.inuits.io/repository/composer/sserbin/twig-linter/3.1.0/sserbin-twig-linter-3.1.0.zip" ]; }
    { name = "symfony_console-6.2.10.0"; filename = "symfony/console/22700a165c8d3a9c884113e73708da1654ac02ec.zip"; sha256 = "bf5a3ca987988059b4f643af0a4e6ba3587422a7d0b250993bb633a43b828626"; urls = [ "https://nexus.inuits.io/repository/composer/symfony/console/v6.2.10/symfony-console-v6.2.10.zip" ]; }
    { name = "symfony_finder-6.2.7.0"; filename = "symfony/finder/886d2acf64f6f21753f4c3cccecb5cd2a9f09642.zip"; sha256 = "08da08c31c82c1fdc73dbc215c5fca6003fb038a97f40980d8536a745d8b7e84"; urls = [ "https://nexus.inuits.io/repository/composer/symfony/finder/v6.2.7/symfony-finder-v6.2.7.zip" ]; }
    { name = "symfony_polyfill-intl-grapheme-1.27.0.0"; filename = "symfony/polyfill-intl-grapheme/ab1909603cf2a31de1c24de93cef8783281e90f1.zip"; sha256 = "11c409622d2321bfdd2fd1efdbdeaaba95da5211256f6cfdb4e003d7bd7eda45"; urls = [ "https://nexus.inuits.io/repository/composer/symfony/polyfill-intl-grapheme/v1.27.0/symfony-polyfill-intl-grapheme-v1.27.0.zip" ]; }
    { name = "symfony_polyfill-intl-normalizer-1.27.0.0"; filename = "symfony/polyfill-intl-normalizer/e122f840325cba6856b7d6518586fdc59dba13c4.zip"; sha256 = "22bf30d7b50aabe1846e3ffbc03011903e05f4208d3bb49b373c4e5fe09500b5"; urls = [ "https://nexus.inuits.io/repository/composer/symfony/polyfill-intl-normalizer/v1.27.0/symfony-polyfill-intl-normalizer-v1.27.0.zip" ]; }
    { name = "symfony_string-6.2.8.0"; filename = "symfony/string/38c2393cce07411937f448a442d002deee2a8d76.zip"; sha256 = "4616b23fdc3c9003893cb5ce03519daa689ee55f0975cba9e7bbc0c55e7ee890"; urls = [ "https://nexus.inuits.io/repository/composer/symfony/string/v6.2.8/symfony-string-v6.2.8.zip" ]; }
  ];
  localPackages = [
  ];

  # Shell snippet to collect all project dependencies.
  collectCacheScript = writeText "collect-cache.sh" (
    concatMapStrings (args: ''
      (
        cacheFile=${escapeShellArg args.filename}
        cacheFilePath="$COMPOSER_CACHE_DIR/files/$cacheFile"
        mkdir -p "$(dirname "$cacheFilePath")"
        cp ${escapeShellArg (fetcher args)} "$cacheFilePath"
      )
    '') cacheEntries
  );

  replaceLocalPaths = writeText "replace-local-paths.sh" (
    concatMapStrings (args: ''
      sed -i -e "s|\"${args.string}\"|\"${args.path}\"|" composer.lock
    '') localPackages
  );

in stdenv.mkDerivation {
  name = "zarybnicky_tkolymp.cz";
  inherit src;

  # Make sure the build uses the right PHP version everywhere.
  # Also include unzip for Composer.
  buildInputs = [ php unzip ];

  # Defines the shell alias to run Composer.
  postHook = ''
    composer () {
      php "$NIX_COMPOSER_PATH" "$@"
    }
  '';

  configurePhase = ''
    runHook preConfigure

    # Set the cache directory for Composer.
    export COMPOSER_CACHE_DIR="$NIX_BUILD_TOP/.composer/cache"

    # Build the cache directory contents.
    source ${collectCacheScript}

    # Replace local package paths with their Nix store equivalent.
    source ${replaceLocalPaths}

    # Store the absolute path to Composer for the 'composer' alias.
    export NIX_COMPOSER_PATH="$(readlink -f ${escapeShellArg composerPath})"

    # Run normal Composer install to complete dependency installation.
    composer install

    runHook postConfigure
  '';

  buildPhase = ''
    runHook preBuild
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p $out/libexec $out/bin

    # Move the entire project to the output directory.
    mv $PWD "$out/libexec/$sourceRoot"
    cd "$out/libexec/$sourceRoot"

    # Update the path to Composer.
    export NIX_COMPOSER_PATH="$(readlink -f ${escapeShellArg composerPath})"

    # Invoke a plugin internal command to setup binaries.
    composer nixify-install-bin "$out/bin"

    runHook postInstall
  '';

  passthru = {
    inherit php;
  };
}