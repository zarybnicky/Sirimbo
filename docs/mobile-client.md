# Mobilní klient

Repozitář nyní obsahuje oficiální Expo aplikaci v pracovním prostoru `mobile`.

## První spuštění

```bash
yarn install
```

## Vývojové příkazy

```bash
yarn workspace rozpisovnik-mobile start
```

Spustí Metro bundler a nabídne QR kód pro připojení zařízení nebo emulátoru.

```bash
yarn workspace rozpisovnik-mobile android
```

Sestaví nativní balíček a spustí jej v Android emulátoru/připojeném zařízení.

```bash
yarn workspace rozpisovnik-mobile ios
```

Sestaví aplikaci pro iOS (vyžaduje macOS s Xcode).

```bash
yarn workspace rozpisovnik-mobile web
```

Spustí klienta v prohlížeči s využitím Expo web.

```bash
yarn workspace rozpisovnik-mobile typecheck
```

Provede TypeScript kontrolu bez generování výstupu.

## Konfigurace prostředí

Expo klient používá následující proměnné prostředí (předpony `EXPO_PUBLIC_` jsou dostupné na klientovi):

- `EXPO_PUBLIC_GRAPHQL_BACKEND` – URL GraphQL serveru (výchozí `http://localhost:3000`).
- `EXPO_PUBLIC_TENANT_ID` – identifikátor tenanta posílaný v hlavičce `x-tenant-id` (výchozí `1`).
- `EXPO_PUBLIC_MOBILE_VERSION` – volitelná identifikace verze klienta posílaná při načtení uživatele (výchozí `mobile-client`).

Při změně tenantů nebo backendu nezapomeňte aplikaci restartovat, aby se znovu vytvořil URQL klient s novými hlavičkami.

## Grafické assety

Ikony a splash screen nejsou v repozitáři uložené. Před vydáním aplikace je vygenerujte z designových podkladů a uložte do `mobile/assets/`, poté aktualizujte `app.json`, pokud bude potřeba.
