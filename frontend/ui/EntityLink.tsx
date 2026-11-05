import { cn } from '@/lib/cn';
import { slugify } from '@/lib/slugify';
import { formatParticipant } from '@/ui/format';
import Link from 'next/link';
import type { ComponentProps } from 'react';

type EntityProps =
  | {
      type: 'person' | 'couple' | 'location' | 'cohort';
      entity: { id: string; name: string };
    }
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
        firstName?: string | null;
        lastName?: string | null;
      };
    };

type EntityLinkProps = Omit<ComponentProps<typeof Link>, 'href' | 'type'> & EntityProps & {
  fallback?: boolean;
};

function getHref(props: EntityProps): string | undefined {
  switch (props.type) {
    case 'federation-person':
      if (!props.entity.externalId || props.entity.externalId === '0') return undefined;
      return `/federated/${props.entity.federation}/people/${props.entity.externalId}`;
    case 'federation-competitor':
      if (!props.entity.externalId || props.entity.externalId === '0') return undefined;
      return `/federated/${props.entity.federation}/competitors/${props.entity.externalId}`;
    case 'person':
      return `/clenove/${props.entity.id}`;
    case 'couple':
      return `/pary/${props.entity.id}`;
    case 'cohort':
      return `/treninkove-skupiny/${props.entity.id}/${slugify(props.entity.name)}`;
    case 'participant':
      return props.entity.person
        ? `/clenove/${props.entity.person.id}`
        : props.entity.couple
          ? `/pary/${props.entity.couple.id}`
          : undefined;
    case 'location':
    case 'event-location': {
      const location = props.type === 'event-location' ? props.entity.location : props.entity;
      return location ? `/lokality/${location.id}` : undefined;
    }
  }
}

export function EntityLink(props: EntityLinkProps) {
  const { type, entity, children, className, fallback = true, ...linkProps } = props;
  const href = getHref(props);
  let content = children;
  if (content == null) {
    switch (type) {
      case 'event-location':
        content = entity.location?.name || entity.locationText;
        break;
      case 'participant':
        content = formatParticipant(entity);
        break;
      case 'federation-person':
        content = entity.canonicalName || entity.name || entity.externalId;
        break;
      case 'federation-competitor':
        content = entity.name || entity.externalId;
        break;
      default:
        content = entity.name;
    }
  }

  if (!href) return !fallback ? null : <>{content}</>;

  return (
    <Link
      {...linkProps}
      href={href}
      className={cn(
        'underline hover:text-accent-11 focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-accent-8',
        className,
      )}
    >
      {content}
    </Link>
  );
}
