import { FileListDocument, type FileFragment } from '@/graphql/File';
import { cn } from '@/lib/cn';
import { Dialog, DialogContent, DialogTitle, DialogTrigger } from '@/ui/dialog';
import { FieldHelper, FieldLabel } from '@/ui/form';
import { InputGroup } from '@/ui/fields/text';
import { buttonCls, inputCls } from '@/ui/style';
import {
  Check,
  FileText,
  FolderOpen,
  LockOpen,
  Paperclip,
  Search,
  Upload,
  X,
} from 'lucide-react';
import Image from 'next/image';
import React from 'react';
import {
  type Control,
  type FieldValues,
  type Path,
  useController,
} from 'react-hook-form';
import { useQuery } from 'urql';

type Props = {
  value: string[];
  onChange: (value: string[]) => void;
};

type UploadedFile = Pick<FileFragment, 'id' | 'name' | 'url'>;

const uploadedAtFormatter = new Intl.DateTimeFormat('cs-CZ', {
  dateStyle: 'short',
  timeStyle: 'short',
});

function formatFileSize(value: string) {
  const bytes = Number(value);
  if (bytes < 1_000_000) return `${Math.ceil(bytes / 1000)} kB`;
  return `${(bytes / 1_000_000).toLocaleString('cs-CZ', {
    maximumFractionDigits: 1,
  })} MB`;
}

async function uploadFile(source: File): Promise<UploadedFile> {
  const response = await fetch('/f', {
    method: 'POST',
    headers: {
      'content-type': source.type || 'application/octet-stream',
      'x-file-name': encodeURIComponent(source.name),
    },
    body: source,
  });
  if (!response.ok) throw new Error('Upload failed');
  return response.json();
}

export function FilePicker({ value, onChange }: Props) {
  const [{ data, fetching }, refresh] = useQuery({ query: FileListDocument });
  const [uploading, setUploading] = React.useState(false);
  const [error, setError] = React.useState<string>();

  const files = (data?.files?.nodes ?? []).filter((file) => file.uploadedAt);
  const selected = new Set(value);
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

    const results = await Promise.allSettled(sources.map(uploadFile));

    const uploadedIds = results.flatMap((x) =>
      x.status === 'fulfilled' ? [x.value.id] : [],
    );
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
          isSelected={(file) => selected.has(file.id)}
          onSelect={(file) =>
            onChange(
              selected.has(file.id)
                ? value.filter((id) => id !== file.id)
                : [...value, file.id],
            )
          }
        >
          <button type="button" className={buttonCls({ variant: 'outline', size: 'sm' })}>
            <FolderOpen />
            Vybrat existující
          </button>
        </FileLibrary>
      </div>

      {error && <p className="text-sm text-danger-11">{error}</p>}
    </section>
  );
}

export function ImageUrlField<T extends FieldValues>({
  control,
  name,
  label,
}: {
  control: Control<T>;
  name: Path<T>;
  label: React.ReactNode;
}) {
  const { field, fieldState } = useController({ control, name });
  const [{ data, fetching }, refresh] = useQuery({ query: FileListDocument });
  const [uploading, setUploading] = React.useState(false);
  const [libraryOpen, setLibraryOpen] = React.useState(false);
  const [error, setError] = React.useState<string>();
  const value = typeof field.value === 'string' ? field.value : '';
  const files = (data?.files?.nodes ?? []).filter(
    (file) => file.uploadedAt && file.contentType?.startsWith('image/'),
  );

  const upload = async (input: FileList | null) => {
    const source = input?.[0];
    if (!source) return;

    setUploading(true);
    setError(undefined);
    try {
      const uploaded = await uploadFile(source);
      field.onChange(uploaded.url);
      refresh({ requestPolicy: 'network-only' });
    } catch {
      setError('Obrázek se nepodařilo nahrát.');
    } finally {
      setUploading(false);
    }
  };

  return (
    <div>
      <FieldLabel htmlFor={name}>{label}</FieldLabel>
      <InputGroup>
        <input
          ref={field.ref}
          id={name}
          name={field.name}
          value={value}
          onBlur={field.onBlur}
          onChange={(event) => {
            setError(undefined);
            field.onChange(event);
          }}
          inputMode="url"
          aria-invalid={fieldState.invalid || undefined}
          className={inputCls({ className: 'min-w-0 grow' })}
        />

        <label
          className={cn(
            buttonCls({
              variant: 'outline',
              size: 'none',
              className: 'w-10 shrink-0 cursor-pointer [&_svg]:size-4',
            }),
            uploading && 'pointer-events-none text-neutral-9 opacity-60',
          )}
          title="Nahrát obrázek"
        >
          <Upload aria-hidden="true" />
          <span className="sr-only">
            {uploading ? 'Nahrávám obrázek' : 'Nahrát obrázek'}
          </span>
          <input
            className="sr-only"
            type="file"
            accept="image/*"
            disabled={uploading}
            onChange={(event) => {
              void upload(event.currentTarget.files);
              event.currentTarget.value = '';
            }}
          />
        </label>

        <FileLibrary
          open={libraryOpen}
          onOpenChange={setLibraryOpen}
          title="Vybrat obrázek"
          files={files}
          fetching={fetching}
          isSelected={(file) => file.url === value}
          onSelect={(file) => {
            setError(undefined);
            field.onChange(file.url);
            setLibraryOpen(false);
          }}
        >
          <button
            type="button"
            className={buttonCls({
              variant: 'outline',
              size: 'none',
              className: 'w-10 shrink-0 [&_svg]:size-4',
            })}
            title="Vybrat existující obrázek"
          >
            <FolderOpen aria-hidden="true" />
            <span className="sr-only">Vybrat existující obrázek</span>
          </button>
        </FileLibrary>
      </InputGroup>

      <FieldHelper error={fieldState.error} helperText={error} />
      {value && <ImagePreview key={value} src={value} />}
    </div>
  );
}

function ImagePreview({ src }: { src: string }) {
  const [failed, setFailed] = React.useState(false);

  return (
    <div className="relative mt-2 h-32 max-w-sm">
      {failed ? (
        <p className="text-sm text-danger-11">Náhled obrázku se nepodařilo načíst.</p>
      ) : (
        <Image
          fill
          sizes="24rem"
          className="rounded-md object-contain object-left"
          src={src}
          alt="Náhled hlavní fotky"
          onError={() => setFailed(true)}
        />
      )}
    </div>
  );
}

function FileLibrary({
  files,
  fetching,
  isSelected,
  onSelect,
  title = 'Vybrat soubory',
  children,
  open,
  onOpenChange,
}: {
  files: FileFragment[];
  fetching: boolean;
  isSelected: (file: FileFragment) => boolean;
  onSelect: (file: FileFragment) => void;
  title?: string;
  children: React.ReactElement;
  open?: boolean;
  onOpenChange?: (open: boolean) => void;
}) {
  const [search, setSearch] = React.useState('');
  const query = search.trim().toLocaleLowerCase();
  const visibleFiles = files.filter(
    (file) =>
      !query ||
      file.name.toLocaleLowerCase().includes(query) ||
      file.displayName?.toLocaleLowerCase().includes(query),
  );

  return (
    <Dialog open={open} onOpenChange={onOpenChange}>
      <DialogTrigger.Plain asChild>{children}</DialogTrigger.Plain>

      <DialogContent className="grid-rows-[auto_auto_minmax(0,1fr)] sm:max-w-3xl">
        <DialogTitle>{title}</DialogTitle>

        <label className="block">
          <span className="sr-only">Hledat soubory</span>
          <InputGroup>
            <span className="inline-flex items-center border border-accent-7 bg-accent-2 px-3 text-accent-10">
              <Search className="size-4" aria-hidden="true" />
            </span>
            <input
              autoFocus
              type="search"
              value={search}
              onChange={(event) => setSearch(event.currentTarget.value)}
              placeholder="Hledat podle názvu"
              className={inputCls({ className: 'min-w-0 grow' })}
            />
          </InputGroup>
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
                const selected = isSelected(file);
                return (
                  <li key={file.id}>
                    <button
                      type="button"
                      aria-pressed={selected}
                      className={cn(
                        'relative flex size-full flex-col overflow-hidden rounded-md border bg-neutral-2 text-left outline-hidden',
                        selected
                          ? 'border-accent-9 ring-2 ring-accent-7'
                          : 'border-neutral-5 hover:border-neutral-7 focus-visible:border-accent-8 focus-visible:ring-2 focus-visible:ring-accent-7',
                      )}
                      onClick={() => onSelect(file)}
                    >
                      <span className="relative flex aspect-4/3 w-full shrink-0 items-center justify-center overflow-hidden bg-neutral-3">
                        {file.contentType?.startsWith('image/') ? (
                          <Image
                            fill
                            src={file.url}
                            alt=""
                            sizes="(min-width: 768px) 11rem, (min-width: 640px) 33vw, 50vw"
                            className="object-cover"
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
                        <span className="mt-0.5 flex flex-wrap gap-x-1 text-[11px] leading-tight text-neutral-9">
                          {file.uploadedAt && (
                            <time dateTime={file.uploadedAt}>
                              {uploadedAtFormatter.format(new Date(file.uploadedAt))}
                            </time>
                          )}
                          {file.byteSize && (
                            <>
                              <span aria-hidden="true">·</span>
                              <span>{formatFileSize(file.byteSize)}</span>
                            </>
                          )}
                          {file.isPublic && (
                            <>
                              <span aria-hidden="true">·</span>
                              <span className="inline-flex items-center" title="Veřejný soubor">
                                <LockOpen className="size-3" aria-hidden="true" />
                                <span className="sr-only">Veřejný soubor</span>
                              </span>
                            </>
                          )}
                        </span>
                      </span>
                      {selected && (
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
