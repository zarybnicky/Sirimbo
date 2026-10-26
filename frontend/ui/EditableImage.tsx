import Image from '@/ui/Image';
import { cn } from '@/lib/cn';
import { ImageIcon, Pencil } from 'lucide-react';

export function EditableImage({
  src,
  onEdit,
  className,
  sizes,
}: {
  src?: string | null;
  onEdit?: () => void;
  className: string;
  sizes: string;
}) {
  if (!src && !onEdit) return null;
  return (
    <div className="relative w-fit max-w-full shrink-0">
      {src ? (
        <a
          href={src}
          target="_blank"
          rel="noreferrer"
          aria-label="Zobrazit fotografii"
          className="block overflow-hidden rounded-md"
        >
          <Image
            src={src}
            alt=""
            width={160}
            height={160}
            sizes={sizes}
            className={cn('size-auto', className)}
          />
        </a>
      ) : (
        <div className="flex size-20 items-center justify-center rounded-md bg-neutral-3 text-neutral-9">
          <ImageIcon className="size-7" aria-hidden="true" />
        </div>
      )}
      {onEdit && (
        <button
          type="button"
          aria-label="Změnit fotografii"
          title="Změnit fotografii"
          onClick={onEdit}
          className="absolute -right-1 -bottom-1 flex size-7 items-center justify-center rounded-full bg-neutral-1 text-neutral-12 shadow-md hover:bg-neutral-4 focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-accent-8"
        >
          <Pencil className="size-3.5" aria-hidden="true" />
        </button>
      )}
    </div>
  );
}
