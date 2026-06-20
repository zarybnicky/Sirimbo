import { cn } from '@/lib/cn';
import { ExternalLink } from 'lucide-react';

export function CstsPersonLink({
  idt,
  className,
  children,
}: {
  idt?: number | string | null;
  className?: string;
  children: React.ReactNode;
}) {
  const id = Number(idt);
  if (!id) return null;

  return (
    <a
      href={`https://www.csts.cz/dancesport/evidence/lide/${id}/osobni_udaje`}
      target="_blank"
      rel="noreferrer"
      title="Otevřít osobu na ČSTS"
      className={cn(
        'inline-flex gap-1 items-baseline rounded-sm text-sm text-neutral-9 hover:text-accent-11 focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-accent-8',
        className,
      )}
    >
      {children}
      <ExternalLink className="size-3.5" aria-hidden="true" />
    </a>
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
  if (!event || !competition) return null;

  return (
    <a
      href={`https://www.csts.cz/dancesport/vysledky_soutezi/event/${event}/competition/${competition}`}
      target="_blank"
      rel="noreferrer"
      title="Otevřít výsledky na ČSTS"
      className={cn(
        'inline-flex gap-1 items-baseline rounded-sm text-sm text-neutral-9 hover:text-accent-11 focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-accent-8',
        className,
      )}
    >
      {children}
      <ExternalLink className="size-3.5" aria-hidden="true" />
    </a>
  );
}
