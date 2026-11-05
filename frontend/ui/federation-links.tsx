import { cn } from '@/lib/cn';
import { EntityLink } from '@/ui/EntityLink';
import { ExternalLink } from 'lucide-react';
import React from 'react';

export function LocalPersonLinks({ people }: { people: { id: string; name: string }[] }) {
  return (
    <>
      {people.map((person, index) => (
        <span key={person.id}>
          {index > 0 && ', '}
          <EntityLink type="person" entity={person} className="text-accent-11" />
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
  if (!eventId) return null;
  return (
    <a
      href={`https://www.csts.cz/dancesport/vysledky_soutezi/event/${eventId}${competitionId ? `/competition/${competitionId}` : ''}`}
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

export function CstsRegistrationLink({
  eventId,
  extraId,
  children,
}: {
  eventId: string;
  extraId: string;
  children: React.ReactNode;
}) {
  if (!eventId || !extraId) return null;
  return (
    <a
      href={`https://www.csts.cz/dancesport/kalendar_akci/event/${eventId}/competition/${extraId}/prihlaseni`}
      target="_blank"
      rel="noreferrer"
      className="inline-flex gap-1 items-baseline rounded-xs text-sm text-neutral-9 underline hover:text-accent-11 focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-accent-8"
    >
      {children}
      <ExternalLink className="size-3.5" aria-hidden="true" />
    </a>
  );
}
