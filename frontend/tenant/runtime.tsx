"use client"

import dynamic from "next/dynamic"
import type { ComponentType, ReactNode } from "react"
import { createContext, useContext, useEffect, useMemo, useState } from "react"

import {
  findTenantByHost,
  tenancyCatalog,
  type TenancyCatalogEntry,
  type TenantComponentLoader,
  type TenantComponentRegistry,
} from "./catalog"

type TenancyContextValue = {
  tenant: TenancyCatalogEntry
  host: string | null
}

const defaultTenant = tenancyCatalog[0]

const TenancyContext = createContext<TenancyContextValue>({
  tenant: defaultTenant,
  host: null,
})

type TenantProviderProps = {
  children: ReactNode
  initialHost?: string | null
}

const resolveBrowserHost = () => {
  if (typeof window === "undefined") {
    return null
  }

  return window.location.host.toLowerCase()
}

export function TenantProvider({ children, initialHost }: TenantProviderProps) {
  const normalizedInitialHost = initialHost?.trim().toLowerCase() ?? null
  const [host, setHost] = useState<string | null>(() => {
    return normalizedInitialHost ?? resolveBrowserHost()
  })

  useEffect(() => {
    if (!normalizedInitialHost) {
      setHost((currentHost) => currentHost ?? resolveBrowserHost())
      return
    }

    setHost((currentHost) => (currentHost === normalizedInitialHost ? currentHost : normalizedInitialHost))
  }, [normalizedInitialHost])

  const value = useMemo(() => {
    const tenant = findTenantByHost(host) ?? defaultTenant
    return { tenant, host }
  }, [host])

  return <TenancyContext.Provider value={value}>{children}</TenancyContext.Provider>
}

export function useTenancy() {
  return useContext(TenancyContext)
}

export function useTenant() {
  return useTenancy().tenant
}

const componentCache = new WeakMap<TenantComponentLoader, ComponentType<unknown>>()

const getDynamicComponent = (loader: TenantComponentLoader) => {
  let Component = componentCache.get(loader)
  if (!Component) {
    Component = dynamic(loader as () => Promise<{ default: ComponentType<unknown> }>, {
      ssr: true,
      loading: () => null,
    }) as ComponentType<unknown>
    componentCache.set(loader, Component)
  }
  return Component
}

export const useTenantComponent = <TName extends keyof TenantComponentRegistry>(name: TName) => {
  const tenant = useTenant()
  const loader = tenant.components[name]

  return useMemo(() => {
    if (!loader) return null
    return getDynamicComponent(loader)
  }, [loader])
}

export function TenantSeo() {
  const Component = useTenantComponent("seo")
  return Component ? <Component /> : null
}

export function SidebarLogo() {
  const Component = useTenantComponent("sidebarLogo")
  return Component ? <Component /> : null
}

export function DesktopLogo() {
  const Component = useTenantComponent("desktopLogo")
  return Component ? <Component /> : null
}

export function MobileLogo() {
  const Component = useTenantComponent("mobileLogo")
  return Component ? <Component /> : null
}

export function Sponsors() {
  const Component = useTenantComponent("sponsors")
  return Component ? <Component /> : null
}

export function SocialIcons() {
  const Component = useTenantComponent("socialIcons")
  return Component ? <Component /> : null
}
