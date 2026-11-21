'use client';

import { GlobalSearchDocument, type GlobalSearchQuery } from '@/graphql/Search';
import { useAuth } from '@/lib/auth';
import { useMemberMenu } from '@/lib/use-menu';
import { getEntityLink } from '@/ui/EntityLink';
import { Dialog, DialogContent, DialogTitle, DialogTrigger } from '@/ui/dialog';
import * as DialogPrimitive from '@radix-ui/react-dialog';
import { buttonCls, inputCls } from '@/ui/style';
import { useFormResult } from '@/ui/form';
import {
  dateTimeFormatter,
  formatAddress,
  formatCount,
  formatEventType,
  formatOpenDateRange,
} from '@/ui/format';
import { formatAgeGroup } from '@/ui/csts';
import { rankItem } from '@tanstack/match-sorter-utils';
import { Command } from 'cmdk';
import {
  Search,
  X,
  CalendarDays,
  UserRound,
  UsersRound,
  MapPin,
  PanelsTopLeft,
} from 'lucide-react';
import { useRouter } from 'next/navigation';
import React from 'react';
import { useQuery } from 'urql';

export function GlobalSearch({
  mobile = false,
  children,
}: {
  mobile?: boolean;
  children?: React.ReactNode;
}) {
  const container = React.useRef<HTMLDivElement>(null);
  React.useEffect(() => {
    const onKey = (event: KeyboardEvent) => {
      if (
        !event.defaultPrevented &&
        (event.metaKey || event.ctrlKey) &&
        event.key.toLowerCase() === 'k' &&
        container.current?.getClientRects().length
      ) {
        event.preventDefault();
        container.current
          .querySelector<HTMLButtonElement>('[aria-haspopup="dialog"]')
          ?.click();
      }
    };
    document.addEventListener('keydown', onKey);
    return () => document.removeEventListener('keydown', onKey);
  }, []);
  return (
    <div
      ref={container}
      className={mobile ? 'group relative grid lg:hidden' : 'hidden lg:block px-2 pb-3'}
    >
      <Dialog>
        {mobile ? (
          children
        ) : (
          <DialogTrigger.Plain
            className={buttonCls({
              variant: 'none',
              display: 'listItem',
              size: 'none',
              className:
                'gap-2 rounded-2xl px-3 py-1.5 text-sm tracking-wider hover:bg-accent-10 hover:text-white active:bg-accent-11 active:text-white data-[state=open]:bg-accent-10',
            })}
          >
            <Search aria-hidden className="size-4 shrink-0" />
            <span>Hledat</span>
            <kbd className="ml-auto text-xs tracking-normal opacity-60">⌘ / Ctrl K</kbd>
          </DialogTrigger.Plain>
        )}
        {mobile ? (
          <DialogPrimitive.Content
            aria-describedby={undefined}
            className="group/search relative col-start-1 row-start-1 min-w-0 data-[state=open]:animate-overlayShow data-[state=closed]:animate-overlayHide [animation-duration:220ms] motion-reduce:animate-none"
          >
            <DialogTitle className="sr-only">Hledat v klubu</DialogTitle>
            <SearchContents mobile />
          </DialogPrimitive.Content>
        ) : (
          <DialogContent
            showCloseButton={false}
            className="p-0 gap-0 sm:max-w-2xl overflow-hidden"
          >
            <DialogTitle className="sr-only">Hledat v klubu</DialogTitle>
            <SearchContents />
          </DialogContent>
        )}
      </Dialog>
    </div>
  );
}

function SearchContents({ mobile = false }: { mobile?: boolean }) {
  const { onSuccess: close } = useFormResult();
  const auth = useAuth();
  const router = useRouter();
  const menu = useMemberMenu();

  const [search, setSearch] = React.useState('');
  const [debounced, setDebounced] = React.useState('');
  React.useEffect(() => {
    const timer = setTimeout(() => setDebounced(search.trim()), 100);
    return () => clearTimeout(timer);
  }, [search]);

  const [{ data, fetching, error }, retry] = useQuery({
    query: GlobalSearchDocument,
    variables: { search: debounced },
    pause: debounced.length < 3,
    requestPolicy: 'network-only',
  });

  const navigation = [
    {
      title: auth.isLoggedIn ? 'Můj profil' : 'Přihlásit se',
      href: auth.isLoggedIn ? '/profil' : '/login',
    },
    ...menu.flatMap((item) => (item.type === 'link' ? [item] : item.children)),
  ].filter(
    ({ href, title }, index, items) =>
      items.findIndex((x) => x.href === href) === index &&
      (search.trim()
        ? rankItem(title, search.trim()).passed
        : ['/profil', '/login', '/rozpis', '/clenove'].includes(href.split('?', 1)[0]!)),
  );
  const current = search.trim() === debounced;
  const results =
    current && debounced.length >= 3 && !fetching && !error
      ? (data?.globalSearchList.toSorted((a, b) => a.position - b.position) ?? [])
      : [];
  const navigate = (href: string) => {
    close();
    router.push(href);
  };
  const itemClass = buttonCls({
    display: 'listItem',
    variant: 'none',
    size: 'none',
    className:
      'gap-3 rounded-xs p-3 text-sm cursor-pointer hover:bg-accent-5 active:bg-accent-6 data-[selected=true]:bg-accent-5 data-[selected=true]:text-accent-12 data-[selected=true]:active:bg-accent-6',
  });
  return (
    <Command
      shouldFilter={false}
      label="Hledat v klubu"
      className={mobile ? 'h-full' : undefined}
    >
      <div
        className={
          mobile
            ? 'flex h-full items-center gap-2 p-3'
            : 'flex items-center gap-2 border-b border-neutral-4 p-3'
        }
      >
        <div
          className={
            mobile
              ? 'relative min-w-0 flex-1 group-data-[state=open]/search:animate-searchExpand group-data-[state=closed]/search:animate-searchCollapse motion-reduce:animate-none'
              : 'relative min-w-0 flex-1'
          }
        >
          <Search
            aria-hidden
            className="pointer-events-none absolute left-3 top-1/2 size-4 -translate-y-1/2 text-accent-10"
          />
          <Command.Input
            value={search}
            onValueChange={setSearch}
            placeholder="Hledat v klubu…"
            className={inputCls({ className: 'h-10 min-w-0 pl-10' })}
          />
        </div>
        {mobile && (
          <DialogPrimitive.Close
            aria-label="Zavřít hledání"
            className={buttonCls({
              display: 'compact',
              variant: 'none',
              size: 'lg',
              className: 'hover:bg-white/10 active:bg-white/20',
            })}
          >
            <X />
          </DialogPrimitive.Close>
        )}
      </div>
      <Command.List
        className={
          mobile
            ? 'absolute inset-x-0 top-full max-h-[65dvh] overflow-y-auto overscroll-contain border-t border-neutral-4 bg-neutral-1 p-2 text-neutral-12 shadow-lg group-data-[state=open]/search:animate-slideDownAndFade motion-reduce:animate-none'
            : 'max-h-[65dvh] overflow-y-auto overscroll-contain p-2'
        }
      >
        {!search.trim() && (
          <p className="px-3 pt-3 pb-4 text-sm text-neutral-11">
            Hledejte lidi, páry, skupiny nebo události.
          </p>
        )}
        {navigation.length > 0 && (
          <Command.Group
            heading={search.trim() ? 'Přejít na' : 'Návrhy'}
            className="**:[[cmdk-group-heading]]:px-3 **:[[cmdk-group-heading]]:py-2 **:[[cmdk-group-heading]]:text-xs **:[[cmdk-group-heading]]:text-neutral-11"
          >
            {navigation.map((item) => (
              <Command.Item
                key={item.href}
                value={item.href}
                onSelect={() => navigate(item.href)}
                className={itemClass}
              >
                {React.createElement(
                  item.href.startsWith('/rozpis') || item.href === '/akce'
                    ? CalendarDays
                    : item.href.startsWith('/profil') || item.href === '/login'
                      ? UserRound
                      : ['/clenove', '/pary', '/treninkove-skupiny'].includes(item.href)
                        ? UsersRound
                        : PanelsTopLeft,
                  { className: 'size-4 shrink-0 text-neutral-11', 'aria-hidden': true },
                )}
                {item.title}
              </Command.Item>
            ))}
          </Command.Group>
        )}
        {results.length > 0 && (
          <Command.Group
            heading="Výsledky"
            className="**:[[cmdk-group-heading]]:px-3 **:[[cmdk-group-heading]]:py-2 **:[[cmdk-group-heading]]:text-xs **:[[cmdk-group-heading]]:text-neutral-11"
          >
            {results.map((result) => {
              const summary = getSearchResultSummary(result);
              if (!summary) return null;
              const { title, href, detail, icon: Icon } = summary;
              return (
                <Command.Item
                  key={href}
                  value={href}
                  onSelect={() => navigate(href)}
                  className={itemClass}
                >
                  <Icon className="size-4 shrink-0 text-neutral-11" aria-hidden />
                  <span className="min-w-0 flex-1">
                    <span className="block truncate">{title}</span>
                    <span className="block text-xs text-neutral-11">{detail}</span>
                  </span>
                </Command.Item>
              );
            })}
          </Command.Group>
        )}
        {search.trim() && (
          <div role="status" className="text-sm text-neutral-11 p-3">
            {search.trim().length < 3 ? (
              'Napište alespoň 3 znaky.'
            ) : !current || fetching ? (
              'Hledám…'
            ) : error ? (
              <button onClick={() => retry({ requestPolicy: 'network-only' })}>
                Hledání se nezdařilo. Zkusit znovu
              </button>
            ) : results.length === 0 ? (
              'Nic jsme nenašli.'
            ) : null}
          </div>
        )}
      </Command.List>
      <div className="hidden lg:flex justify-between border-t border-neutral-4 px-6 py-3 text-xs text-neutral-11">
        <span>↑ ↓ výběr · Enter otevřít</span>
        <span>Esc zavřít</span>
      </div>
    </Command>
  );
}

function getSearchResultSummary(result: GlobalSearchQuery['globalSearchList'][number]) {
  if (result.person) {
    const person = result.person;
    return {
      ...getEntityLink('person', person),
      icon: UserRound,
      detail: [
        person.isAdmin && 'Správce',
        person.isTrainer && 'Trenér',
        person.isMember && 'Člen',
        person.isExternal && 'Externí',
        formatAgeGroup(person.ageGroup),
        ...person.cohortMembershipsList.map((x) => x.cohort?.name),
        ...person.couplesList
          .flatMap((x) => [x.man, x.woman])
          .flatMap((x) => (x && x.id !== person.id ? `tančí s ${x.name}` : [])),
      ]
        .filter(Boolean)
        .join(', '),
    };
  }
  if (result.couple) {
    const couple = result.couple;
    return {
      ...getEntityLink('couple', couple),
      icon: UsersRound,
      detail: [formatAgeGroup(couple.ageGroup), formatOpenDateRange(couple)]
        .filter(Boolean)
        .join(', '),
    };
  }
  if (result.cohort) {
    const cohort = result.cohort;
    return {
      ...getEntityLink('cohort', cohort),
      icon: UsersRound,
      detail: [
        cohort.isArchived && 'Archivovaná',
        !cohort.isVisible && 'Neveřejná',
        formatCount(cohort.cohortMembershipsList.length, {
          one: 'člen',
          few: 'členové',
          other: 'členů',
        }),
        cohort.location,
      ]
        .filter(Boolean)
        .join(', '),
    };
  }
  if (result.location) {
    return {
      ...getEntityLink('location', result.location),
      detail: formatAddress(result.location.address) || 'Místo',
      icon: MapPin,
    };
  }
  if (result.event) {
    const event = result.event;
    return {
      ...getEntityLink('event', event),
      detail: [
        event.isCancelled && 'Zrušeno',
        dateTimeFormatter.format(new Date(event.since)),
        event.location?.name || event.locationText,
      ]
        .filter(Boolean)
        .join(', '),
      icon: CalendarDays,
    };
  }
  return null;
}
