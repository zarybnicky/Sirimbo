import { cn } from '@/lib/cn';
import { useAuth } from '@/lib/auth';
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
  const { isSystemAdmin } = useAuth();
  if (!externalId || externalId === '0') return null;
  const href = isSystemAdmin
    ? `/admin/federated/${federation}/people/${externalId}`
    : federation === 'csts'
      ? `https://www.csts.cz/dancesport/evidence/lide/${externalId}/osobni_udaje`
      : null;
  if (!href) return <span className={className}>{children}</span>;

  return (
    <Link
      href={href}
      target={isSystemAdmin ? undefined : '_blank'}
      rel={isSystemAdmin ? undefined : 'noreferrer'}
      className={cn(
        'inline-flex gap-1 items-baseline rounded-xs text-sm text-neutral-9 hover:text-accent-11 focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-accent-8',
        className,
      )}
    >
      {children}
      {!isSystemAdmin && <ExternalLink className="size-3.5" aria-hidden="true" />}
    </Link>
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
        'inline-flex gap-1 items-baseline rounded-xs text-sm text-neutral-9 hover:text-accent-11 focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-accent-8',
        className,
      )}
    >
      {children}
      <ExternalLink className="size-3.5" aria-hidden="true" />
    </a>
  );
}
