import type { EventSavedSearchFragment } from '@/graphql/Event';
import { Dialog, DialogContent, DialogTitle } from '@/ui/dialog';
import {
  DropdownMenu,
  DropdownMenuButton,
  DropdownMenuContent,
  DropdownMenuTrigger,
} from '@/ui/dropdown';
import { EventSavedSearchForm } from '@/ui/forms/EventSavedSearchForm';
import { buttonCls } from '@/ui/style';
import { Check, ChevronDown, Plus, Settings } from 'lucide-react';
import React from 'react';

export function EventSavedSearchPicker({
  savedSearches,
  value,
  onChange,
}: {
  savedSearches: readonly EventSavedSearchFragment[];
  value: string | null;
  onChange: (id: string | null) => void;
}) {
  const [open, setOpen] = React.useState(false);
  const [editingId, setEditingId] = React.useState<string | null | undefined>();
  const editing = savedSearches.find((x) => x.id === editingId);
  const selected = savedSearches.find((x) => x.id === value);

  if (savedSearches.length === 0) return null;

  return (
    <>
      <DropdownMenu open={open} onOpenChange={setOpen}>
        <DropdownMenuTrigger className={buttonCls({ variant: 'outline', size: 'sm', className: 'mb-2 self-start' })}>
          Uložené filtry{selected ? `: ${selected.name}` : ''} <ChevronDown />
        </DropdownMenuTrigger>
        <DropdownMenuContent
          align="start"
          onCloseAutoFocus={(event) => {
            if (editingId !== undefined) event.preventDefault();
          }}
        >
          <DropdownMenuButton onSelect={() => onChange(null)}>
            <Check className={value === null ? 'size-4' : 'size-4 invisible'} />
            Moje události
          </DropdownMenuButton>
          {savedSearches.map((search) => (
            <DropdownMenuButton
              key={search.id}
              textValue={search.name}
              onSelect={() => onChange(search.id)}
              className="py-0 pr-0"
            >
              <Check className={value === search.id ? 'size-4 shrink-0' : 'size-4 shrink-0 invisible'} />
              <span className="grow py-3">{search.name}</span>
              <button
                type="button"
                aria-label={`Upravit filtr ${search.name}`}
                title="Upravit filtr"
                className="self-stretch rounded-r-[3px] border-l border-neutral-4 px-3 hover:bg-accent-5 hover:text-accent-12 focus-visible:outline-2 focus-visible:outline-accent-9"
                onPointerUp={(event) => event.stopPropagation()}
                onClick={(event) => {
                  event.stopPropagation();
                  setEditingId(search.id);
                  setOpen(false);
                }}
              >
                <Settings className="size-4" />
              </button>
            </DropdownMenuButton>
          ))}
          <DropdownMenuButton
            className="mt-1 border-t border-neutral-4"
            onSelect={() => setEditingId(null)}
          >
            <Plus className="size-4" /> Vytvořit nový
          </DropdownMenuButton>
        </DropdownMenuContent>
      </DropdownMenu>

      {editingId !== undefined && (
        <Dialog open onOpenChange={() => setEditingId(undefined)}>
          <DialogContent>
            <DialogTitle>{editing ? 'Upravit uložený filtr' : 'Nový uložený filtr'}</DialogTitle>
            <EventSavedSearchForm savedSearch={editing} />
          </DialogContent>
        </Dialog>
      )}
    </>
  );
}
