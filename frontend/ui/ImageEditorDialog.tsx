'use client';

import Image from '@/ui/Image';
import { Dialog, DialogContent, DialogTitle } from '@/ui/dialog';
import { ImageLibraryDialog, uploadFile } from '@/ui/forms/FilePicker';
import { buttonCls } from '@/ui/style';
import { FolderOpen, ImageIcon, Trash2, Upload } from 'lucide-react';
import React from 'react';

type SelectedImage = { id: string; url: string };

export function ImageEditorDialog({
  file,
  onSave,
  onClose,
}: {
  file?: SelectedImage | null;
  onSave: (id: string | null) => Promise<void>;
  onClose: () => void;
}) {
  const input = React.useRef<HTMLInputElement>(null);
  const [selection, setSelection] = React.useState(file ?? null);
  const [libraryOpen, setLibraryOpen] = React.useState(false);
  const [uploading, setUploading] = React.useState(false);
  const [saving, setSaving] = React.useState(false);
  const [error, setError] = React.useState<string>();

  const upload = async (source: File | undefined) => {
    if (!source) return;
    setUploading(true);
    setError(undefined);
    try {
      setSelection(await uploadFile(source));
    } catch {
      setError('Fotografii se nepodařilo nahrát.');
    } finally {
      setUploading(false);
    }
  };

  return (
    <Dialog
      open
      onOpenChange={(open) => {
        if (!open) onClose();
      }}
    >
      <DialogContent className="sm:max-w-md">
        <DialogTitle>Fotografie</DialogTitle>
        <form
          className="space-y-4"
          onSubmit={async (event) => {
            event.preventDefault();
            setSaving(true);
            setError(undefined);
            try {
              await onSave(selection?.id ?? null);
              onClose();
            } catch (error) {
              setError(
                error instanceof Error ? error.message : 'Změnu se nepodařilo uložit.',
              );
            } finally {
              setSaving(false);
            }
          }}
        >
          <div className="relative flex h-64 items-center justify-center">
            {selection ? (
              <Image
                src={selection.url}
                alt="Náhled fotografie"
                fill
                sizes="400px"
                className="object-contain"
              />
            ) : (
              <ImageIcon className="size-12 text-neutral-8" aria-hidden="true" />
            )}
          </div>

          <fieldset
            disabled={uploading || saving}
            className="flex flex-wrap justify-center gap-2"
          >
            <button
              type="button"
              className={buttonCls({ variant: 'outline', size: 'sm' })}
              onClick={() => input.current?.click()}
            >
              <Upload />
              {uploading ? 'Nahrávám…' : 'Nahrát'}
            </button>
            <input
              ref={input}
              hidden
              type="file"
              accept="image/*"
              onChange={(event) => {
                void upload(event.currentTarget.files?.[0]);
                event.currentTarget.value = '';
              }}
            />
            <button
              type="button"
              className={buttonCls({ variant: 'outline', size: 'sm' })}
              onClick={() => setLibraryOpen(true)}
            >
              <FolderOpen />Z knihovny
            </button>
            {selection && (
              <button
                type="button"
                className={buttonCls({ variant: 'outline', size: 'sm' })}
                onClick={() => setSelection(null)}
              >
                <Trash2 />
                Odebrat
              </button>
            )}
          </fieldset>

          {error && (
            <p role="alert" className="text-sm text-danger-11">
              {error}
            </p>
          )}
          <div className="flex justify-end gap-2">
            <button
              type="button"
              className={buttonCls({ variant: 'outline' })}
              onClick={onClose}
            >
              Zrušit
            </button>
            <button
              type="submit"
              className={buttonCls()}
              disabled={uploading || saving || selection?.id === file?.id}
            >
              {saving ? 'Ukládám…' : 'Uložit'}
            </button>
          </div>
        </form>

        {libraryOpen && (
          <ImageLibraryDialog
            open
            onOpenChange={setLibraryOpen}
            onSelect={setSelection}
          />
        )}
      </DialogContent>
    </Dialog>
  );
}
