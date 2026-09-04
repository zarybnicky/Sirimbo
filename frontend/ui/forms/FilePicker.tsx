/* eslint-disable @next/next/no-img-element */
import { FileListDocument, type FileFragment } from '@/graphql/File';
import { cn } from '@/lib/cn';
import { Dialog, DialogContent, DialogTitle, DialogTrigger } from '@/ui/dialog';
import { buttonCls } from '@/ui/style';
import { Check, FileText, FolderOpen, Paperclip, Search, Upload, X } from 'lucide-react';
import React from 'react';
import { useQuery } from 'urql';

type Props = {
  value: string[];
  onChange: (value: string[]) => void;
};

export function FilePicker({ value, onChange }: Props) {
  const [{ data, fetching }, refresh] = useQuery({ query: FileListDocument });
  const [uploading, setUploading] = React.useState(false);
  const [error, setError] = React.useState<string>();

  const files = (data?.files?.nodes ?? []).filter((file) => file.uploadedAt);
  const filesById = new Map(files.map((file) => [file.id, file]));
  const selectedFiles = value.flatMap((id) => {
    const file = filesById.get(id);
    return file ? [file] : [];
  });

  const upload = async (input: FileList | null) => {
    const sources = [...(input ?? [])];
    if (sources.length === 0) return;

    setUploading(true);
    setError(undefined);

    const results = await Promise.allSettled(
      sources.map(async (source) => {
        const response = await fetch('/f', {
          method: 'POST',
          headers: {
            'content-type': source.type || 'application/octet-stream',
            'x-file-name': encodeURIComponent(source.name),
          },
          body: source,
        });
        if (!response.ok) throw new Error('Upload failed');
        return (await response.json()) as { id: string };
      }),
    );

    const uploadedIds = results.flatMap((x) => x.status === 'fulfilled' ? [x.value.id] : []);
    if (uploadedIds.length > 0) {
      onChange([...new Set([...value, ...uploadedIds])]);
      refresh({ requestPolicy: 'network-only' });
    }
    if (uploadedIds.length !== sources.length) {
      setError('Některé soubory se nepodařilo nahrát.');
    }
    setUploading(false);
  };

  return (
    <section className="space-y-2 rounded-md border border-neutral-6 bg-neutral-1 p-2">
      <h3 className="text-sm font-semibold text-neutral-12">Přílohy</h3>

      {selectedFiles.length > 0 && (
        <ul className="divide-y divide-neutral-5 rounded-md border border-neutral-5">
          {selectedFiles.map((file) => (
            <li key={file.id} className="flex items-center gap-2 px-2 py-1.5 text-sm">
              <Paperclip className="size-4 shrink-0 text-neutral-10" />
              <a
                className="min-w-0 grow truncate text-accent-11 hover:underline"
                href={file.url}
                target="_blank"
                rel="noreferrer"
              >
                {file.displayName ?? file.name}
              </a>
              <button
                type="button"
                className="rounded-sm p-1 text-neutral-10 hover:bg-neutral-4 hover:text-neutral-12"
                aria-label={`Odebrat ${file.displayName ?? file.name}`}
                onClick={() => onChange(value.filter((id) => id !== file.id))}
              >
                <X className="size-4" />
              </button>
            </li>
          ))}
        </ul>
      )}

      <div className="flex flex-wrap gap-2">
        <label
          className={cn(
            buttonCls({ variant: 'outline', size: 'sm' }),
            uploading && 'pointer-events-none opacity-60',
          )}
        >
          <Upload />
          {uploading ? 'Nahrávám…' : 'Nahrát soubory'}
          <input
            className="sr-only"
            type="file"
            multiple
            disabled={uploading}
            onChange={(event) => {
              void upload(event.currentTarget.files);
              event.currentTarget.value = '';
            }}
          />
        </label>

        <FileLibrary
          files={files}
          fetching={fetching}
          value={value}
          onChange={onChange}
        />
      </div>

      {error && <p className="text-sm text-danger-11">{error}</p>}
    </section>
  );
}

function FileLibrary({
  files,
  fetching,
  value,
  onChange,
}: {
  files: FileFragment[];
  fetching: boolean;
  value: string[];
  onChange: (value: string[]) => void;
}) {
  const [search, setSearch] = React.useState('');
  const selected = new Set(value);
  const query = search.trim().toLocaleLowerCase();
  const visibleFiles = files.filter(
    (file) =>
      !query ||
      file.name.toLocaleLowerCase().includes(query) ||
      file.displayName?.toLocaleLowerCase().includes(query),
  );

  return (
    <Dialog>
      <DialogTrigger.Plain asChild>
        <button
          type="button"
          className={buttonCls({ variant: 'outline', size: 'sm' })}
        >
          <FolderOpen />
          Vybrat existující
        </button>
      </DialogTrigger.Plain>

      <DialogContent className="grid-rows-[auto_auto_minmax(0,1fr)] sm:max-w-3xl">
        <DialogTitle>Vybrat soubory</DialogTitle>

        <label className="relative block">
          <span className="sr-only">Hledat soubory</span>
          <Search className="absolute left-3 top-1/2 size-4 -translate-y-1/2 text-neutral-10" />
          <input
            autoFocus
            type="search"
            value={search}
            onChange={(event) => setSearch(event.currentTarget.value)}
            placeholder="Hledat podle názvu"
            className="w-full rounded-md border border-neutral-6 bg-neutral-2 py-2 pl-9 pr-3 text-sm text-neutral-12 outline-hidden focus:border-accent-8 focus:ring-2 focus:ring-accent-7"
          />
        </label>

        <div className="min-h-48 overflow-y-auto overscroll-contain">
          {fetching && visibleFiles.length === 0 ? (
            <p className="py-12 text-center text-sm text-neutral-10">Načítám…</p>
          ) : visibleFiles.length === 0 ? (
            <p className="py-12 text-center text-sm text-neutral-10">
              Žádné soubory neodpovídají hledání.
            </p>
          ) : (
            <ul className="grid grid-cols-2 gap-2 sm:grid-cols-3 md:grid-cols-4">
              {visibleFiles.map((file) => {
                const isSelected = selected.has(file.id);
                return (
                  <li key={file.id}>
                    <button
                      type="button"
                      aria-pressed={isSelected}
                      className={cn(
                        'relative flex size-full flex-col overflow-hidden rounded-md border bg-neutral-2 text-left outline-hidden',
                        isSelected
                          ? 'border-accent-9 ring-2 ring-accent-7'
                          : 'border-neutral-5 hover:border-neutral-7 focus-visible:border-accent-8 focus-visible:ring-2 focus-visible:ring-accent-7',
                      )}
                      onClick={() =>
                        onChange(
                          isSelected
                            ? value.filter((id) => id !== file.id)
                            : [...value, file.id],
                        )
                      }
                    >
                      <span className="flex aspect-4/3 w-full items-center justify-center bg-neutral-3">
                        {file.contentType?.startsWith('image/') ? (
                          <img
                            src={file.url}
                            alt=""
                            loading="lazy"
                            className="size-full object-cover"
                          />
                        ) : (
                          <FileText className="size-10 text-neutral-9" />
                        )}
                      </span>
                      <span className="w-full p-2">
                        <span className="block truncate text-sm font-medium text-neutral-12">
                          {file.displayName ?? file.name}
                        </span>
                        {file.displayName && (
                          <span className="block truncate text-xs text-neutral-10">
                            {file.name}
                          </span>
                        )}
                      </span>
                      {isSelected && (
                        <span className="absolute right-2 top-2 rounded-full bg-accent-9 p-1 text-accent-0 shadow-sm">
                          <Check className="size-3" />
                        </span>
                      )}
                    </button>
                  </li>
                );
              })}
            </ul>
          )}
        </div>
      </DialogContent>
    </Dialog>
  );
}
