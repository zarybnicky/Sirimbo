import { cn } from '@/lib/cn';
import Link from 'next/link';
import { ExternalLink } from 'lucide-react';

export function FederationLink({
  entity,
  federation,
  externalId,
  className,
  children,
}: {
  entity: 'people' | 'competitors';
  federation: string;
  externalId?: number | string | null;
  className?: string;
  children: React.ReactNode;
}) {
  if (!externalId || externalId === '0') return null;

  return (
    <Link
      href={`/federated/${federation}/${entity}/${externalId}`}
      className={cn(
        'inline-flex gap-1 items-baseline rounded-xs text-sm text-neutral-11 underline hover:text-accent-12 focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-accent-8',
        className,
      )}
    >
      {children}
    </Link>
  );
}

export function LocalPersonLinks({ people }: { people: { id: string; name: string }[] }) {
  return (
    <>
      {people.map((person, index) => (
        <span key={person.id}>
          {index > 0 && ', '}
          <Link className="text-accent-11 underline" href={`/clenove/${person.id}`}>
            {person.name}
          </Link>
        </span>
      ))}
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
