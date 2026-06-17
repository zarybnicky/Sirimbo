import { cn } from '@/lib/cn';
import { cstsPersonUrl } from '@/ui/csts';
import { ExternalLink } from 'lucide-react';

export function CstsPersonSourceLink({
  idt,
  className,
}: {
  idt?: number | string | null;
  className?: string;
}) {
  const href = cstsPersonUrl(idt);
  if (!href) return null;

  return (
    <a
      href={href}
      target="_blank"
      rel="noreferrer"
      aria-label="Otevřít osobu na ČSTS"
      title="Otevřít osobu na ČSTS"
      className={cn(
        'inline-flex size-5 items-center justify-center rounded-sm text-neutral-9 hover:text-accent-11 focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-accent-8',
        className,
      )}
    >
      <ExternalLink className="size-3.5" aria-hidden="true" />
    </a>
  );
}
