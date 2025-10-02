"use client"

import { useMemo } from 'react'

import { useTenant } from '@/tenant/runtime';
import type { LinkProps } from 'next/link'
type Route = LinkProps['href'];

export type MenuLink = {
  type: 'link';
  title: string;
  href: Route;
  requireTrainer?: boolean;
  requireAdmin?: boolean;
  className?: string;
};

export type MenuStructItem =
  | {
      type: 'menu';
      title: string;
      children: MenuLink[];
    }
  | MenuLink;

export function getHrefs(x: MenuStructItem): Route[] {
  return x.type === 'link' ? [x.href] : x.children.flatMap((x) => getHrefs(x));
}

const baseTopMenu: MenuStructItem[] = [
  { type: 'link', title: 'Domů', href: '/' },
  {
    type: 'menu',
    title: 'Klub',
    children: [
      { type: 'link', title: 'O nás', href: '/o-nas' },
      { type: 'link', title: 'Kde trénujeme', href: '/kde-trenujeme' },
      { type: 'link', title: 'Tréninkové programy', href: '/treninkove-programy' },
      { type: 'link', title: 'Trenéři', href: '/treneri' },
      { type: 'link', title: 'Výhody členství', href: '/vyhody-clenstvi' },
      { type: 'link', title: 'Galerie mistrů', href: '/galerie-mistru' },
    ],
  },
  {
    type: 'menu',
    title: 'Nabízíme',
    children: [
      { type: 'link', title: 'Přípravka tanečního sportu', href: 'https://nabor.tkolymp.cz' as any },
      { type: 'link', title: 'Vystoupení na akcích', href: '/vystoupeni' },
      { type: 'link', title: 'Školní taneční kroužky', href: '/skolni-krouzky' },
    ],
  },
  { type: 'link', title: 'Aktuality', href: '/clanky' },
  { type: 'link', title: 'Galerie', href: '/galerie' },
  { type: 'link', title: 'Akce', href: '/akce' },
  { type: 'link', title: 'Kontakt', href: '/kontakt' },
]

export function useTopMenu() {
  const tenant = useTenant()

  return useMemo(() => {
    if (tenant.features.articles) {
      return baseTopMenu
    }

    return baseTopMenu.filter((item) => item.type !== 'link' || item.title !== 'Aktuality')
  }, [tenant])
}

export function useMemberMenu() {
  const tenant = useTenant()

  return useMemo(() => {
    const items: MenuStructItem[] = [
      {
        type: 'link',
        title: 'Nástěnka',
        href: {
          pathname: '/dashboard',
          query: { tab: 'myAnnouncements' },
        },
      },
    ]

    if (tenant.features.articles) {
      items.push({
        type: 'link',
        title: 'Stálá nástěnka',
        className: 'lg:hidden',
        href: {
          pathname: '/dashboard',
          query: { tab: 'myAnnouncements' },
        },
      })
    }

    items.push(
      { type: 'link', title: 'Profil', href: '/profil' },
      {
        type: 'menu',
        title: 'Tréninky',
        children: [
          {
            type: 'link',
            title: 'Moje tréninky',
            href: {
              pathname: '/dashboard',
              query: { tab: 'myLessons' },
            },
          },
          { type: 'link', title: 'Kalendář', href: '/rozpis' },
          { type: 'link', title: 'Seznam akcí', href: '/akce' },
        ],
      },
      {
        type: 'menu',
        title: 'Taneční klub',
        children: [
          { type: 'link', title: 'Klub', href: '/tanecni-klub' },
          { type: 'link', title: 'Tréninkové skupiny', href: '/treninkove-skupiny' },
          { type: 'link', title: 'Páry', href: '/pary' },
          { type: 'link', title: 'Členové', href: '/clenove' },
          { type: 'link', title: 'Žebříček', href: '/zebricek' },
          ...(tenant.features.articles
            ? ([{ type: 'link', title: 'Dokumenty', href: '/dokumenty' }] as MenuLink[])
            : []),
        ],
      },
      {
        type: 'menu',
        title: 'Správa',
        children: [
          { type: 'link', title: 'Pozvánky', href: '/pozvanky', requireAdmin: true },
          { type: 'link', title: 'Nástěnka', href: '/nastenka', requireTrainer: true },
          { type: 'link', title: 'Platby', href: '/platby', requireAdmin: true },
          ...(tenant.features.articles
            ? ([
                { type: 'link', title: 'Články', href: '/aktuality', requireTrainer: true },
                { type: 'link', title: 'Vyplněné formuláře', href: '/crm', requireAdmin: true },
                { type: 'link', title: 'Upload (WIP)', href: '/upload', requireAdmin: true },
              ] as MenuLink[])
            : []),
          ...(tenant.slug === 'starlet'
            ? ([
                {
                  type: 'link',
                  title: 'Import z evidence',
                  href: '/starlet-import',
                  requireAdmin: true,
                },
              ] as MenuLink[])
            : []),
        ],
      },
    )

    return items
  }, [tenant])
}
