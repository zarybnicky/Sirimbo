/* eslint-disable @next/next/no-img-element */
import type { FileFragment } from '@/graphql/File';
import { Paperclip } from 'lucide-react';

export function FileAttachments({
  attachments,
}: {
  attachments: ReadonlyArray<{ file: FileFragment | null }>;
}) {
  const files = attachments.flatMap(({ file }) => (file ? [file] : []));
  const images = files.filter((file) => file.contentType?.startsWith('image/'));
  const documents = files.filter((file) => !file.contentType?.startsWith('image/'));
  if (files.length === 0) return null;

  return (
    <div className="mt-4 border-t border-neutral-5 pt-3">
      <h4 className="mb-1 text-xs font-semibold uppercase tracking-wide text-neutral-10">
        Přílohy
      </h4>

      {images.length > 0 && (
        <ul className="mb-2 grid gap-2 sm:grid-cols-2">
          {images.map((file) => (
            <li key={file.id}>
              <a
                className="block overflow-hidden rounded-md border border-neutral-5 bg-neutral-2 text-accent-11 hover:border-accent-7 focus-visible:outline-hidden focus-visible:ring-2 focus-visible:ring-accent-8"
                href={file.url}
                target="_blank"
                rel="noreferrer"
              >
                <img
                  className="aspect-4/3 w-full object-contain"
                  src={file.url}
                  alt={file.displayName ?? file.name}
                  loading="lazy"
                />
                <span className="block truncate border-t border-neutral-5 px-2 py-1.5 text-sm">
                  {file.displayName ?? file.name}
                </span>
              </a>
            </li>
          ))}
        </ul>
      )}

      {documents.length > 0 && (
        <ul className="space-y-1">
          {documents.map((file) => (
            <li key={file.id}>
              <a
                className="inline-flex max-w-full items-center gap-2 text-sm text-accent-11 hover:underline"
                href={file.url}
                target="_blank"
                rel="noreferrer"
              >
                <Paperclip className="size-4 shrink-0" />
                <span className="truncate">{file.displayName ?? file.name}</span>
              </a>
            </li>
          ))}
        </ul>
      )}
    </div>
  );
}
