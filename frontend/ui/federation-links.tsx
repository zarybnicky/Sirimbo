import { cn } from '@/lib/cn';
import Link from 'next/link';
import { ExternalLink } from 'lucide-react';

export function FederationPersonLink({
  federation,
  externalId,
  className,
  children,
}: {
  federation: string;
  externalId?: number | string | null;
  className?: string;
  children: React.ReactNode;
}) {
  if (!externalId || externalId === '0') return null;

  return (
    <Link
      href={`/federated/${federation}/people/${externalId}`}
      className={cn(
        'inline-flex gap-1 items-baseline rounded-xs text-sm text-neutral-9 underline hover:text-accent-11 focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-accent-8',
        className,
      )}
    >
      {children}
    </Link>
  );
}

export function FederationCompetitorLink({
  federation,
  externalId,
  className,
  children,
}: {
  federation: string;
  externalId?: number | string | null;
  className?: string;
  children: React.ReactNode;
}) {
  if (!externalId || externalId === '0') return null;

  return (
    <Link
      href={`/federated/${federation}/competitors/${externalId}`}
      className={cn(
        'inline-flex gap-1 items-baseline rounded-xs text-sm text-neutral-9 underline hover:text-accent-11 focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-accent-8',
        className,
      )}
    >
      {children}
    </Link>
  );
}

export function LocalPersonLinks({
  federation,
  externalId,
  people,
}: {
  federation: string;
  externalId: string;
  people: { totalCount: number; nodes: { id: string; name: string }[] };
}) {
  return (
    <>
      {people.nodes.map((person, index) => (
        <span key={person.id}>
          {index > 0 && ', '}
          <Link className="text-accent-11 underline" href={`/clenove/${person.id}`}>
            {person.name}
          </Link>
        </span>
      ))}
      {people.totalCount > people.nodes.length && (
        <FederationPersonLink
          federation={federation}
          externalId={externalId}
          className="ml-1 text-accent-11"
        >
          All {people.totalCount} matches
        </FederationPersonLink>
      )}
    </>
  );
}

export function CstsResultsLink({
  eventId,
  competitionId,
  className,
  children,
}: {
  eventId?: number | string | null;
  competitionId?: number | string | null;
  className?: string;
  children: React.ReactNode;
}) {
  const event = Number(eventId);
  const competition = Number(competitionId);
  if (!event || (competitionId != null && !competition)) return null;

  return (
    <a
      href={`https://www.csts.cz/dancesport/vysledky_soutezi/event/${event}${competition ? `/competition/${competition}` : ''}`}
      target="_blank"
      rel="noreferrer"
      title="Otevřít výsledky na ČSTS"
      className={cn(
        'inline-flex gap-1 items-baseline rounded-xs text-sm text-neutral-9 underline hover:text-accent-11 focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-accent-8',
        className,
      )}
    >
      {children}
      <ExternalLink className="size-3.5" aria-hidden="true" />
    </a>
  );
}
