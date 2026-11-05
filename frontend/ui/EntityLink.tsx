import { cn } from '@/lib/cn';
import { slugify } from '@/lib/slugify';
import { formatParticipant } from '@/ui/format';
import Link from 'next/link';
import type { ComponentProps } from 'react';

type LinkedEntityProps =
  | {
      type: 'event';
      entity: { id: string; name: string | null; isCancelled?: boolean };
    }
  | {
      type: 'person' | 'location' | 'cohort';
      entity: { id: string; name: string };
    }
  | {
      type: 'couple';
      entity: { id: string; shortName?: string; name?: string };
    };

type EntityProps =
  | LinkedEntityProps
  | {
      type: 'participant';
      entity: {
        person: { id: string; name: string } | null;
        couple: { id: string; shortName: string } | null;
      };
    }
  | {
      type: 'event-location';
      entity: {
        location?: { id: string; name: string } | null;
        locationText?: string | null;
      };
    }
  | {
      type: 'federation-person';
      entity: {
        federation: string;
        externalId?: string | number | null;
        name?: string | null;
        canonicalName?: string | null;
      };
    }
  | {
      type: 'federation-competitor';
      entity: {
        federation: string;
        externalId?: string | number | null;
        name?: string | null;
        canonicalName?: string | null;
      };
    };

type EntityLinkProps = Omit<ComponentProps<typeof Link>, 'href' | 'type'> &
  EntityProps & {
    fallback?: boolean;
    tab?: string;
  };

type EntityLinkArgs<Props = EntityProps> = Props extends EntityProps
  ? [type: Props['type'], entity: Props['entity']]
  : never;

export function getEntityLink(...args: EntityLinkArgs<LinkedEntityProps>): {
  title: string;
  href: string;
};
export function getEntityLink(...args: EntityLinkArgs): { title: string; href?: string };
export function getEntityLink(...[type, entity]: EntityLinkArgs): {
  title: string;
  href?: string;
} {
  switch (type) {
    case 'federation-person':
      return {
        title: entity.canonicalName || entity.name || String(entity.externalId) || '',
        href:
          entity.externalId && entity.externalId !== '0'
            ? `/federated/${entity.federation}/people/${entity.externalId}`
            : undefined,
      };
    case 'federation-competitor':
      return {
        title: entity.name || String(entity.externalId) || '',
        href:
          entity.externalId && entity.externalId !== '0'
            ? `/federated/${entity.federation}/competitors/${entity.externalId}`
            : undefined,
      };
    case 'event':
      return { title: entity.name ?? '', href: `/termin/${entity.id}` };
    case 'person':
      return { title: entity.name, href: `/clenove/${entity.id}` };
    case 'couple':
      return { title: entity.name || entity.shortName || '', href: `/pary/${entity.id}` };
    case 'cohort':
      return {
        title: entity.name,
        href: `/treninkove-skupiny/${entity.id}/${slugify(entity.name)}`,
      };
    case 'participant':
      return {
        title: formatParticipant(entity),
        href: entity.person
          ? getEntityLink('person', entity.person).href
          : entity.couple
            ? getEntityLink('couple', entity.couple).href
            : undefined,
      };
    case 'location':
      return { title: entity.name, href: `/lokality/${entity.id}` };
    case 'event-location':
      return {
        title: entity.location?.name || entity.locationText || '',
        href: entity.location
          ? getEntityLink('location', entity.location).href
          : undefined,
      };
  }
}

export function EntityLink(props: EntityLinkProps) {
  const { type, entity, children, className, fallback = true, tab, ...linkProps } = props;
  const { title, href } = getEntityLink(...([type, entity] as EntityLinkArgs));
  const content = children ?? title;

  if (!href) return !fallback ? null : <>{content}</>;

  return (
    <Link
      {...linkProps}
      href={tab ? `${href}?${new URLSearchParams({ tab })}` : href}
      className={cn(
        'underline decoration-current/50 hover:decoration-current hover:text-accent-11 focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-accent-8',
        type === 'event' && entity.isCancelled && 'line-through',
        className,
      )}
    >
      {content}
    </Link>
  );
}
