import type { ComponentType } from "react"

export type TenantComponentLoader = () => Promise<{
  default: ComponentType<unknown>
}>

export type TenantComponentRegistry = Partial<{
  seo: TenantComponentLoader
  sidebarLogo: TenantComponentLoader
  desktopLogo: TenantComponentLoader
  mobileLogo: TenantComponentLoader
  sponsors: TenantComponentLoader
  socialIcons: TenantComponentLoader
}>

export type TenantTheme = {
  primary: string
  accent: string
  neutral: string
  accentLight?: Record<string, string>
  accentDark?: Record<string, string>
  neutralLight?: Record<string, string>
  neutralDark?: Record<string, string>
}

export type TenantDisplay = {
  shortName: string
  legalName: string
  favicon: string
  logos: {
    default: string
    dark?: string
    mark?: string
  }
}

export type TenantFeatureToggles = {
  home: boolean
  articles: boolean
}

export type TenancyCatalogEntry = {
  id: number
  slug: string
  name: string
  hosts: string[]
  display: TenantDisplay
  theme: TenantTheme
  features: TenantFeatureToggles
  components: TenantComponentRegistry
}

const createTenantComponentLoader = <TModule extends Record<string, ComponentType<unknown>>, TName extends keyof TModule>(
  importer: () => Promise<TModule>,
  exportName: TName,
): TenantComponentLoader => {
  return async () => {
    const module = await importer()
    const Component = module[exportName]

    if (!Component) {
      throw new Error(`Missing tenant UI export: ${String(exportName)}`)
    }

    return { default: Component }
  }
}

const loadOlympUi = () => import("./olymp/ui")
const loadKometaUi = () => import("./kometa/ui")
const loadStarletUi = () => import("./starlet/ui")

export const tenancyCatalog = [
  {
    id: 1,
    slug: "olymp",
    name: "TK Olymp Olomouc",
    hosts: ["olymp.rozpisovnik.cz", "rozpisovnik.cz"],
    display: {
      shortName: "TK Olymp",
      legalName: "© 2024 TK Olymp Olomouc, z. s.",
      favicon: "/olymp/favicon-32x32.png",
      logos: {
        default: "/olymp/android-chrome-192x192.png",
        dark: "/olymp/android-chrome-512x512.png",
      },
    },
    theme: {
      primary: "#ed1734",
      accent: "red",
      neutral: "gray",
    },
    features: {
      home: true,
      articles: true,
    },
    components: {
      seo: createTenantComponentLoader(loadOlympUi, "TenantSeo"),
      sidebarLogo: createTenantComponentLoader(loadOlympUi, "SidebarLogo"),
      desktopLogo: createTenantComponentLoader(loadOlympUi, "DesktopLogo"),
      mobileLogo: createTenantComponentLoader(loadOlympUi, "MobileLogo"),
      sponsors: createTenantComponentLoader(loadOlympUi, "Sponsors"),
      socialIcons: createTenantComponentLoader(loadOlympUi, "SocialIcons"),
    },
  },
  {
    id: 2,
    slug: "kometa",
    name: "DSP Kometa Brno",
    hosts: ["kometa.rozpisovnik.cz"],
    display: {
      shortName: "DSP Kometa",
      legalName: "© 2024 DSP Kometa Brno, z. s.",
      favicon: "/kometa/favicon-32x32.png",
      logos: {
        default: "/kometa/android-chrome-96x96.png",
        dark: "/kometa/mstile-150x150.png",
        mark: "/kometa/favicon.ico",
      },
    },
    theme: {
      primary: "#be9f69",
      accent: "gold",
      neutral: "mauve",
    },
    features: {
      home: false,
      articles: false,
    },
    components: {
      seo: createTenantComponentLoader(loadKometaUi, "TenantSeo"),
      sidebarLogo: createTenantComponentLoader(loadKometaUi, "SidebarLogo"),
      desktopLogo: createTenantComponentLoader(loadKometaUi, "DesktopLogo"),
      mobileLogo: createTenantComponentLoader(loadKometaUi, "MobileLogo"),
      sponsors: createTenantComponentLoader(loadKometaUi, "Sponsors"),
      socialIcons: createTenantComponentLoader(loadKometaUi, "SocialIcons"),
    },
  },
  {
    id: 3,
    slug: "starlet",
    name: "TK Starlet Brno",
    hosts: ["starlet.rozpisovnik.cz"],
    display: {
      shortName: "TK Starlet",
      legalName: "© 2024 TK Starlet Brno, z. s.",
      favicon: "/starlet/starlet-favicon.webp",
      logos: {
        default: "/starlet/starlet-favicon.webp",
      },
    },
    theme: {
      primary: "#D7A238",
      accent: "orange",
      neutral: "olive",
      accentLight: {
        orange1: "hsl(45, 67%, 99%)",
        orange2: "hsl(46, 100%, 96%)",
        orange3: "hsl(47, 100%, 88%)",
        orange4: "hsl(45, 100%, 83%)",
        orange5: "hsl(43, 100%, 77%)",
        orange6: "hsl(39, 100%, 73%)",
        orange7: "hsl(39, 81%, 68%)",
        orange8: "hsl(40, 70%, 56%)",
        orange9: "hsl(40, 73%, 58%)",
        orange10: "hsl(40, 67%, 53%)",
        orange11: "hsl(41, 100%, 31%)",
        orange12: "hsl(38, 38%, 20%)",
      },
      accentDark: {
        orange1: "hsl(43, 23%, 6%)",
        orange2: "hsl(38, 24%, 9%)",
        orange3: "hsl(39, 48%, 11%)",
        orange4: "hsl(41, 87%, 12%)",
        orange5: "hsl(41, 94%, 14%)",
        orange6: "hsl(41, 77%, 19%)",
        orange7: "hsl(40, 69%, 24%)",
        orange8: "hsl(41, 75%, 30%)",
        orange9: "hsl(40, 73%, 58%)",
        orange10: "hsl(40, 67%, 53%)",
        orange11: "hsl(39, 78%, 61%)",
        orange12: "hsl(38, 78%, 85%)",
      },
    },
    features: {
      home: false,
      articles: false,
    },
    components: {
      seo: createTenantComponentLoader(loadStarletUi, "TenantSeo"),
      sidebarLogo: createTenantComponentLoader(loadStarletUi, "SidebarLogo"),
      desktopLogo: createTenantComponentLoader(loadStarletUi, "DesktopLogo"),
      mobileLogo: createTenantComponentLoader(loadStarletUi, "MobileLogo"),
      sponsors: createTenantComponentLoader(loadStarletUi, "Sponsors"),
      socialIcons: createTenantComponentLoader(loadStarletUi, "SocialIcons"),
    },
  },
] as const satisfies ReadonlyArray<TenancyCatalogEntry>

export type TenancyCatalog = typeof tenancyCatalog

export const tenancyById = new Map(tenancyCatalog.map((tenant) => [tenant.id, tenant] as const))

export const tenancyBySlug = new Map(tenancyCatalog.map((tenant) => [tenant.slug, tenant] as const))

const hostEntries: Array<[string, TenancyCatalogEntry]> = []

for (const tenant of tenancyCatalog) {
  for (const host of tenant.hosts) {
    const normalizedHost = host.toLowerCase()

    if (!hostEntries.some(([knownHost]) => knownHost === normalizedHost)) {
      hostEntries.push([normalizedHost, tenant])
    }
  }
}

export const tenancyByHost = new Map(hostEntries)
