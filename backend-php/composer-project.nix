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
    { name = "clue_stream-filter-1.6.0.0"; filename = "clue/stream-filter/09025d2a9413f6dcf088767f72f19ffc617eca2e.zip"; sha256 = "5a1416db7d03ffb88efddcb3141360fc506d2dbae2b5f0938e8e3c9540c168b6"; urls = [ "https://api.github.com/repos/clue/stream-filter/zipball/d6169430c7731d8509da7aecd0af756a5747b78e" ]; }
    { name = "guzzlehttp_promises-2.0.0.0"; filename = "guzzlehttp/promises/c2fd9faaca50163135fb34a6c3e5299168d281ea.zip"; sha256 = "c800274c615c1933495f35f6cb2d0305904c8d804c2fa0b8579e67ab832415f5"; urls = [ "https://api.github.com/repos/guzzle/promises/zipball/3a494dc7dc1d7d12e511890177ae2d0e6c107da6" ]; }
    { name = "guzzlehttp_psr7-2.5.0.0"; filename = "guzzlehttp/psr7/a0427ce1201a3cb3b789563f888d93bf21d386db.zip"; sha256 = "41fa3379e3d4b5882ba7333cbcbfa37b4ad8d7d1283fe51c8306ac86f280641c"; urls = [ "https://api.github.com/repos/guzzle/psr7/zipball/b635f279edd83fc275f822a1188157ffea568ff6" ]; }
    { name = "http-interop_http-factory-guzzle-1.2.0.0"; filename = "http-interop/http-factory-guzzle/caf524372edc4813e265af9163290061cad63a12.zip"; sha256 = "3141024e30f873361921f6bfd543805a6e9c16cefa98a0027ed6a892ec53de5e"; urls = [ "https://api.github.com/repos/http-interop/http-factory-guzzle/zipball/8f06e92b95405216b237521cc64c804dd44c4a81" ]; }
    { name = "jean85_pretty-package-versions-2.0.5.0"; filename = "jean85/pretty-package-versions/2927fa2b7eafce187111fe3778662baa548ceab3.zip"; sha256 = "089b59065b3b099ba330a322887bc63a1b4188929f9d5ab2af6f17700e85471f"; urls = [ "https://api.github.com/repos/Jean85/pretty-package-versions/zipball/ae547e455a3d8babd07b96966b17d7fd21d9c6af" ]; }
    { name = "php-http_client-common-2.7.0.0"; filename = "php-http/client-common/cbf26998a3b619ea763edfce218f97f8bb0e6018.zip"; sha256 = "f27741e06d36de89f17f2c3b5918ed8189ed689936df0eb8853a9fde1c43889a"; urls = [ "https://api.github.com/repos/php-http/client-common/zipball/880509727a447474d2a71b7d7fa5d268ddd3db4b" ]; }
    { name = "php-http_discovery-1.19.0.0"; filename = "php-http/discovery/d391422acffd8502d195ee3c854895806c3bbb97.zip"; sha256 = "9a9719eec50b253569c3636a53b65b2f3435515b1bf240ec7b08dcbc8ade3c20"; urls = [ "https://api.github.com/repos/php-http/discovery/zipball/1856a119a0b0ba8da8b5c33c080aa7af8fac25b4" ]; }
    { name = "php-http_httplug-2.4.0.0"; filename = "php-http/httplug/d6c2803210dd1aa522aae1a50b5c041d842a22a7.zip"; sha256 = "80d54f34459b3945bd22bcd2966a5fb98c74306f95e8b07a6c88eecb752885b5"; urls = [ "https://api.github.com/repos/php-http/httplug/zipball/625ad742c360c8ac580fcc647a1541d29e257f67" ]; }
    { name = "php-http_message-1.16.0.0"; filename = "php-http/message/3a7450a8b91dd4e67a1e9bcddf0fef2cd916c489.zip"; sha256 = "73c9356fa6f9af74c69ef10c26d5eb51fa057a86bf7b053d0903336762a8c412"; urls = [ "https://api.github.com/repos/php-http/message/zipball/47a14338bf4ebd67d317bf1144253d7db4ab55fd" ]; }
    { name = "php-http_message-factory-1.1.0.0"; filename = "php-http/message-factory/425a52ac6181b9da3e539322baa6e80bd0bf0eac.zip"; sha256 = "2bb2bff915b862ebcb5ceb00b8864059d937e60709e2c790dd15fdb4657c0e0d"; urls = [ "https://api.github.com/repos/php-http/message-factory/zipball/4d8778e1c7d405cbb471574821c1ff5b68cc8f57" ]; }
    { name = "php-http_promise-1.1.0.0"; filename = "php-http/promise/83f5526bc8dced8abb54a7d772e34cadbff53fbe.zip"; sha256 = "7ff1fcd02530d3c97a2a38dd505d53469cb4d87075e59d52c952d36efacd5776"; urls = [ "https://api.github.com/repos/php-http/promise/zipball/4c4c1f9b7289a2ec57cde7f1e9762a5789506f88" ]; }
    { name = "psr_container-2.0.2.0"; filename = "psr/container/a0a43e15cb16e3ab95a991e1bffc813474b15cdf.zip"; sha256 = "3111f5b69d3a0f690f5f92d7aeacfb247f5ebd3c175088cce4b098e5d1b06ad7"; urls = [ "https://api.github.com/repos/php-fig/container/zipball/c71ecc56dfe541dbd90c5360474fbc405f8d5963" ]; }
    { name = "psr_http-client-1.0.2.0"; filename = "psr/http-client/f96d002844893c1942fdf601ec432bebc634906a.zip"; sha256 = "d56c0b59e3d5873b02ed7345194d9d4687c90c2187d4ff824134f2eb28382927"; urls = [ "https://api.github.com/repos/php-fig/http-client/zipball/0955afe48220520692d2d09f7ab7e0f93ffd6a31" ]; }
    { name = "psr_http-factory-1.0.2.0"; filename = "psr/http-factory/d8875dced84085f47fee02b6c9a8c288d7ae271f.zip"; sha256 = "a8cff2adbd84c58c0fe02ed401026c7364c893670352d025f88e060087adf1ef"; urls = [ "https://api.github.com/repos/php-fig/http-factory/zipball/e616d01114759c4c489f93b099585439f795fe35" ]; }
    { name = "psr_http-message-2.0.0.0"; filename = "psr/http-message/e8baaf1c5537971c6192d7c56974d81e2ee099c3.zip"; sha256 = "d561d2c36e2e3c0ee8b54e0e9652b3bfa13cd763e5e6f93400969001f3a7968d"; urls = [ "https://api.github.com/repos/php-fig/http-message/zipball/402d35bcb92c70c026d1a6a9883f06b2ead23d71" ]; }
    { name = "psr_log-3.0.0.0"; filename = "psr/log/4109d4b3f74b78a13cb175029bcb2384f59f0a52.zip"; sha256 = "bbc23516c85615f2f85b707bc10ba26e62033e2afd696e075575b68dc6e31928"; urls = [ "https://api.github.com/repos/php-fig/log/zipball/fe5ea303b0887d5caefd3d431c3e61ad47037001" ]; }
    { name = "ralouphie_getallheaders-3.0.3.0"; filename = "ralouphie/getallheaders/ddc5f4c7ab3d868f5060717336a0d93475d53175.zip"; sha256 = "ef0165da5690da666ab4cb88bb4cec0d50372e3e7c922532c5795a3567b367af"; urls = [ "https://api.github.com/repos/ralouphie/getallheaders/zipball/120b605dfeb996808c31b6477290a714d356e822" ]; }
    { name = "sentry_sdk-3.5.0.0"; filename = "sentry/sdk/b3ced7eb68e732316c207dae9f980db706edc15c.zip"; sha256 = "0dfe6435dfd603926278c3238dc68cfe30b3d33df723551f991f62cee3fe66d9"; urls = [ "https://api.github.com/repos/getsentry/sentry-php-sdk/zipball/cd91b752f07c4bab9fb3b173f81af68a78a78d6d" ]; }
    { name = "sentry_sentry-3.20.1.0"; filename = "sentry/sentry/290a543b9e88a585f456e25041522c369cd1f72a.zip"; sha256 = "1aed2b28be579c108d984a037fa5a0fac9315b62661d25c02ad262f85c233a5d"; urls = [ "https://api.github.com/repos/getsentry/sentry-php/zipball/644ad9768c18139a80ac510090fad000d9ffd8a4" ]; }
    { name = "stephank_composer-plugin-nixify-1.1.0.0"; filename = "stephank/composer-plugin-nixify/6b00aedf28221acbb64a87222a0eb819404901f2.zip"; sha256 = "ac7cc480698f8717fb9fce4077b81303d37fe6ab2b89c8547cd7f9451598ee1e"; urls = [ "https://api.github.com/repos/stephank/composer-plugin-nixify/zipball/d93c4348388d714d7d81b41e34ccb2ae9c2131c2" ]; }
    { name = "symfony_deprecation-contracts-3.3.0.0"; filename = "symfony/deprecation-contracts/3a056d55414a97879a7988336ee8da233ff344ba.zip"; sha256 = "a986294f5e57bff601dd9af7b28b8dfa488c0d7f28266fc05a014b2e48120c5c"; urls = [ "https://api.github.com/repos/symfony/deprecation-contracts/zipball/7c3aff79d10325257a001fcf92d991f24fc967cf" ]; }
    { name = "symfony_http-client-6.3.1.0"; filename = "symfony/http-client/7364841c18cdfc700850f80cb5bfe9ea47a4d1d1.zip"; sha256 = "78be803a8ecfa9b7b9ad8694cb1f852c9a883704c51c37cd1d63b8fd6eafe157"; urls = [ "https://api.github.com/repos/symfony/http-client/zipball/1c828a06aef2f5eeba42026dfc532d4fc5406123" ]; }
    { name = "symfony_http-client-contracts-3.3.0.0"; filename = "symfony/http-client-contracts/ccedfe24e78e7fccd53f08d6893f0ace8be105e9.zip"; sha256 = "1ed82c3398e65924552e5c8637f0a9ac20c47f19cd608c0d66bcf70f1e9cddf9"; urls = [ "https://api.github.com/repos/symfony/http-client-contracts/zipball/3b66325d0176b4ec826bffab57c9037d759c31fb" ]; }
    { name = "symfony_options-resolver-6.3.0.0"; filename = "symfony/options-resolver/0fce1cd5b8db01309a07c80cdd8b7f24090e2136.zip"; sha256 = "021a3bc53c41e93878031d6e44af9e09e3a0045079d8a710d9042140da6f9c8c"; urls = [ "https://api.github.com/repos/symfony/options-resolver/zipball/a10f19f5198d589d5c33333cffe98dc9820332dd" ]; }
    { name = "symfony_polyfill-ctype-1.27.0.0"; filename = "symfony/polyfill-ctype/04e84c03a82a82c09eaa688b17b523570f9d14d2.zip"; sha256 = "66f212bc73088271dbbec1f3b1c3ba21171e4c5ab38676c4d151441f3e38db6f"; urls = [ "https://api.github.com/repos/symfony/polyfill-ctype/zipball/5bbc823adecdae860bb64756d639ecfec17b050a" ]; }
    { name = "symfony_polyfill-mbstring-1.27.0.0"; filename = "symfony/polyfill-mbstring/7061da8bfb2a98acfbdcaf076c3ba5f167d856cc.zip"; sha256 = "b112101451a1f76a0cb4edd6595a5733059fd2f7a4cd4d36052526602e26a8fa"; urls = [ "https://api.github.com/repos/symfony/polyfill-mbstring/zipball/8ad114f6b39e2c98a8b0e3bd907732c207c2b534" ]; }
    { name = "symfony_polyfill-php80-1.27.0.0"; filename = "symfony/polyfill-php80/fe4df98f0392f59ca247eec4d07f380451c09242.zip"; sha256 = "8db38ad9c85c94ad664b89ff9ec9dd5b232dc922f6107bb356d44e9dcf6cde9b"; urls = [ "https://api.github.com/repos/symfony/polyfill-php80/zipball/7a6ff3f1959bb01aefccb463a0f2cd3d3d2fd936" ]; }
    { name = "symfony_service-contracts-3.3.0.0"; filename = "symfony/service-contracts/0ca27cbae8268d042b98a95aeaf4676a075b96af.zip"; sha256 = "64d1429bb730d255ce2f2c2e607bbf745d2f29b1873c0b7a7acd84fe32b313a1"; urls = [ "https://api.github.com/repos/symfony/service-contracts/zipball/40da9cc13ec349d9e4966ce18b5fbcd724ab10a4" ]; }
    { name = "twig_twig-3.6.1.0"; filename = "twig/twig/090b3fa3fbfc153efb8af56fedcc9e6b45125fad.zip"; sha256 = "fe778583c574af8a83aaa936b5a2097dfa4800a51f6bd771f4674782ee6fc045"; urls = [ "https://api.github.com/repos/twigphp/Twig/zipball/7e7d5839d4bec168dfeef0ac66d5c5a2edbabffd" ]; }
    { name = "amphp_amp-2.6.2.0"; filename = "amphp/amp/8c9c2a2bf8be8fda4a26bfe6c6e261d39b6b0320.zip"; sha256 = "c15688dd5756771bb52c1c30f9088327cc49ffb69131183d05b0642e7a4f935f"; urls = [ "https://api.github.com/repos/amphp/amp/zipball/9d5100cebffa729aaffecd3ad25dc5aeea4f13bb" ]; }
    { name = "amphp_byte-stream-1.8.1.0"; filename = "amphp/byte-stream/24823ac0e49e56800f97cb0f4e852334fc3a60e0.zip"; sha256 = "227fbf5650a188d0e7da15b61642bc2a076ed7cd293f20f92205eff862615892"; urls = [ "https://api.github.com/repos/amphp/byte-stream/zipball/acbd8002b3536485c997c4e019206b3f10ca15bd" ]; }
    { name = "composer_package-versions-deprecated-1.11.99.5"; filename = "composer/package-versions-deprecated/7696d631625b248d150b3db3037b6fb36a2f1cf1.zip"; sha256 = "557187e1f72faff2b2e61fedd93824c74b491f6078c8135fe2978488e6ee32c3"; urls = [ "https://api.github.com/repos/composer/package-versions-deprecated/zipball/b4f54f74ef3453349c24a845d22392cd31e65f1d" ]; }
    { name = "composer_pcre-3.1.0.0"; filename = "composer/pcre/16ea22f0a3bddd3c7683dfc2378869ab617d3036.zip"; sha256 = "3aff2a08d578d70c7b1f78553de1962f39a973097dee9bb0e46372358b589b15"; urls = [ "https://api.github.com/repos/composer/pcre/zipball/4bff79ddd77851fe3cdd11616ed3f92841ba5bd2" ]; }
    { name = "composer_semver-3.3.2.0"; filename = "composer/semver/29fa63f52104d4c4bf687735fbaaf52f0a232c4b.zip"; sha256 = "baaee53811af1c73fa17da54c1722c888207008eac70d0f239073fb13e24e56a"; urls = [ "https://api.github.com/repos/composer/semver/zipball/3953f23262f2bff1919fc82183ad9acb13ff62c9" ]; }
    { name = "composer_xdebug-handler-3.0.3.0"; filename = "composer/xdebug-handler/524bae0e78ed4946a977f22dc3ff8629f96dfaf7.zip"; sha256 = "102c6388c487229bbedce718465f5b6bb125c20a655066ae967fcc6ac4cdd0c2"; urls = [ "https://api.github.com/repos/composer/xdebug-handler/zipball/ced299686f41dce890debac69273b47ffe98a40c" ]; }
    { name = "dnoegel_php-xdg-base-dir-0.1.1.0"; filename = "dnoegel/php-xdg-base-dir/b81b493e09050800919750bdd1e829656fc89971.zip"; sha256 = "d881707cf02a8aa1a1846e0166e0b1704eeacac059f08a7ec48b593e3959c40a"; urls = [ "https://api.github.com/repos/dnoegel/php-xdg-base-dir/zipball/8f8a6e48c5ecb0f991c2fdcf5f154a47d85f9ffd" ]; }
    { name = "doctrine_deprecations-1.1.1.0"; filename = "doctrine/deprecations/21dbad56fed7f33b0f3924218184dc3c7745e116.zip"; sha256 = "d115222e70e5259dc111eabc117c3c28eabed8caaf7cc508599efbd6c1241c1d"; urls = [ "https://api.github.com/repos/doctrine/deprecations/zipball/612a3ee5ab0d5dd97b7cf3874a6efe24325efac3" ]; }
    { name = "felixfbecker_advanced-json-rpc-3.2.1.0"; filename = "felixfbecker/advanced-json-rpc/ee93f6a030a8ec4909ae17405ed90c75f9d0c59c.zip"; sha256 = "98b15c828413b99ecd0670f91ee25eea5ac9453d5507b2d63711dabf44982490"; urls = [ "https://api.github.com/repos/felixfbecker/php-advanced-json-rpc/zipball/b5f37dbff9a8ad360ca341f3240dc1c168b45447" ]; }
    { name = "felixfbecker_language-server-protocol-1.5.2.0"; filename = "felixfbecker/language-server-protocol/bee555c8048b240c2c2f28255fef8f3efc814e2c.zip"; sha256 = "67502ffec7d734cd69d724ef6dca52f40dc7f23752d021b61e384656a86d343e"; urls = [ "https://api.github.com/repos/felixfbecker/php-language-server-protocol/zipball/6e82196ffd7c62f7794d778ca52b69feec9f2842" ]; }
    { name = "fidry_cpu-core-counter-0.5.1.0"; filename = "fidry/cpu-core-counter/60c2ca8d9f6d67a847a36c264fe15c3468dd30fa.zip"; sha256 = "3285c95eb8f79bb1ea10d2d57abad41c28d77929840b207c1badf030529d9894"; urls = [ "https://api.github.com/repos/theofidry/cpu-core-counter/zipball/b58e5a3933e541dc286cc91fc4f3898bbc6f1623" ]; }
    { name = "netresearch_jsonmapper-4.2.0.0"; filename = "netresearch/jsonmapper/950f38fd1d77a97f599bdc7d956ce7cca007a585.zip"; sha256 = "05775049f8ab7db98dbf447e919a6a8c8c484df8fcd113be59c32cc76e4976c1"; urls = [ "https://api.github.com/repos/cweiske/jsonmapper/zipball/f60565f8c0566a31acf06884cdaa591867ecc956" ]; }
    { name = "nikic_php-parser-4.16.0.0"; filename = "nikic/php-parser/8dad12acfb0e3f1ed908842c523e7e7e5d683350.zip"; sha256 = "5e83380bfb889b02934646d828754d29ad6e4bd18b2f0987e1e05b83c293358f"; urls = [ "https://api.github.com/repos/nikic/PHP-Parser/zipball/19526a33fb561ef417e822e85f08a00db4059c17" ]; }
    { name = "phpdocumentor_reflection-common-2.2.0.0"; filename = "phpdocumentor/reflection-common/6243ab17e2330fb7ce63d34391d53104cc913dc0.zip"; sha256 = "39fca6dc8730bdc4f70c41e58e16595fc6695b4ae42327c32144c4131410a7f3"; urls = [ "https://api.github.com/repos/phpDocumentor/ReflectionCommon/zipball/1d01c49d4ed62f25aa84a747ad35d5a16924662b" ]; }
    { name = "phpdocumentor_reflection-docblock-5.3.0.0"; filename = "phpdocumentor/reflection-docblock/3c16e706c93df7017840bcf302eace70dc66a077.zip"; sha256 = "3664230b09277d49780b0cb8f0a4a324e061f4bad079ac015b49a3892f7440ef"; urls = [ "https://api.github.com/repos/phpDocumentor/ReflectionDocBlock/zipball/622548b623e81ca6d78b721c5e029f4ce664f170" ]; }
    { name = "phpdocumentor_type-resolver-1.7.2.0"; filename = "phpdocumentor/type-resolver/f123759f027d24940aa17601a04b399409b71265.zip"; sha256 = "66dbbf05cbd211873b8b4ec809e3c7f81e0b8e76695d23c2e0b8d858c35bda1a"; urls = [ "https://api.github.com/repos/phpDocumentor/TypeResolver/zipball/b2fe4d22a5426f38e014855322200b97b5362c0d" ]; }
    { name = "phpstan_phpdoc-parser-1.22.1.0"; filename = "phpstan/phpdoc-parser/b6fd099342943c51070d8f896756c1b730beb8e0.zip"; sha256 = "97ce18a270b25757d752750a1125eb685d09612a2f0eaf0415555017e17604c3"; urls = [ "https://api.github.com/repos/phpstan/phpdoc-parser/zipball/65c39594fbd8c67abfc68bb323f86447bab79cc0" ]; }
    { name = "phpstan_phpstan-1.10.25.0"; filename = "phpstan/phpstan/a8c3fab57ff70584c3e24f2fa39fd76f593d846d.zip"; sha256 = "f1ec09d458202b79039f113944f3faf27686266d20a3d7472b4cdff4acae087f"; urls = [ "https://api.github.com/repos/phpstan/phpstan/zipball/578f4e70d117f9a90699324c555922800ac38d8c" ]; }
    { name = "sebastian_diff-5.0.3.0"; filename = "sebastian/diff/223d74456faf26ca71f008dc25e84f4e691cbfff.zip"; sha256 = "4340570a008415f79a23bbb9d929621f17bac212d5264272bcc71c968018be63"; urls = [ "https://api.github.com/repos/sebastianbergmann/diff/zipball/912dc2fbe3e3c1e7873313cc801b100b6c68c87b" ]; }
    { name = "spatie_array-to-xml-3.1.6.0"; filename = "spatie/array-to-xml/db4b43497ce4f42f74fe57be24de5f5fe54e73ec.zip"; sha256 = "6b1f748873382b1d84d853e540815dd974f156abfc28e0f368d5a9634bedd5f9"; urls = [ "https://api.github.com/repos/spatie/array-to-xml/zipball/e210b98957987c755372465be105d32113f339a4" ]; }
    { name = "sserbin_twig-linter-9999999-dev"; filename = "sserbin/twig-linter/65dcfc87d5ed44188f1af48a0fca4e35b22a5e7c.zip"; sha256 = "a86cf19f13c0beb20426b1d8b4eeafc6d551fba964cb06ef63c2c883a1a545e7"; urls = [ "https://api.github.com/repos/sserbin/twig-linter/zipball/0b7cc4d61b6cf423ec837a0969ea5e0c8f017ddb" ]; }
    { name = "symfony_console-6.3.0.0"; filename = "symfony/console/3be7196f9d1f9e5a31b9b4fca66f1066dfac70e0.zip"; sha256 = "ff4f7e81613e2b3fc0c5ca15aef3c2d2091f656c2cad25aa5477b4d2c56a1e4e"; urls = [ "https://api.github.com/repos/symfony/console/zipball/8788808b07cf0bdd6e4b7fdd23d8ddb1470c83b7" ]; }
    { name = "symfony_filesystem-6.3.1.0"; filename = "symfony/filesystem/195eaec7cc20b26d2a1ce21311c01c19e4f557cf.zip"; sha256 = "ea7a085f127d8060af47c0df3ccb705f602ac04e9ab001cf7c2b947a7c50bec5"; urls = [ "https://api.github.com/repos/symfony/filesystem/zipball/edd36776956f2a6fcf577edb5b05eb0e3bdc52ae" ]; }
    { name = "symfony_finder-6.3.0.0"; filename = "symfony/finder/dd23a3affa93d6d3ad3896829fdad1d5060c123f.zip"; sha256 = "faaa52b01ad118a530e0441eeeadd133939e37844fbff5b22e954e0086a53aec"; urls = [ "https://api.github.com/repos/symfony/finder/zipball/d9b01ba073c44cef617c7907ce2419f8d00d75e2" ]; }
    { name = "symfony_polyfill-intl-grapheme-1.27.0.0"; filename = "symfony/polyfill-intl-grapheme/3e0defb9f075160deb0ea7188610275ee40a9b00.zip"; sha256 = "11c409622d2321bfdd2fd1efdbdeaaba95da5211256f6cfdb4e003d7bd7eda45"; urls = [ "https://api.github.com/repos/symfony/polyfill-intl-grapheme/zipball/511a08c03c1960e08a883f4cffcacd219b758354" ]; }
    { name = "symfony_polyfill-intl-normalizer-1.27.0.0"; filename = "symfony/polyfill-intl-normalizer/3282f96073427a186419bc5ae65aa6726d0f143b.zip"; sha256 = "22bf30d7b50aabe1846e3ffbc03011903e05f4208d3bb49b373c4e5fe09500b5"; urls = [ "https://api.github.com/repos/symfony/polyfill-intl-normalizer/zipball/19bd1e4fcd5b91116f14d8533c57831ed00571b6" ]; }
    { name = "symfony_string-6.3.0.0"; filename = "symfony/string/90249aaa760f009f890781e3adb6c772b3a3aac6.zip"; sha256 = "12d8966a07c6e65cee7418fb61338006da3e15d34ad583632a54fea1b21876fd"; urls = [ "https://api.github.com/repos/symfony/string/zipball/f2e190ee75ff0f5eced645ec0be5c66fac81f51f" ]; }
    { name = "vimeo_psalm-5.13.1.0"; filename = "vimeo/psalm/06df7eb5b3bb1385c4f1bef8b2c0fe8a5cd58756.zip"; sha256 = "5d86150df158ccdae02cf9be52bf434978b18c78811663e2b49792e4c9d4131f"; urls = [ "https://api.github.com/repos/vimeo/psalm/zipball/086b94371304750d1c673315321a55d15fc59015" ]; }
    { name = "webmozart_assert-1.11.0.0"; filename = "webmozart/assert/b9bc0224905e92da07bc3de7d205f17c30035a88.zip"; sha256 = "3dea7b980d4ba44fca74fbd1d068be28f1a771e1a1cb308e89b773eb83fa11a3"; urls = [ "https://api.github.com/repos/webmozarts/assert/zipball/11cb2199493b2f8a3b53e7f19068fc6aac760991" ]; }
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