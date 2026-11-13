import type { EventSearchConditionInput } from '@/graphql';
import { eventTypes } from '@/calendar/state';
import { savedSearchName } from '@/calendar/savedSearchName';
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

const standardViews: {
  id: string | null;
  name: string;
  condition: EventSearchConditionInput;
}[] = [
  {
    id: null,
    name: 'Moje události',
    condition: { onlyMine: true, withCompetitions: true },
  },
];

const conditionDefaults = {
  types: [...eventTypes],
  trainerIds: null,
  locationIds: null,
  participantIds: null,
  parentId: null,
  cohortId: null,
  coupleId: null,
  onlyMine: false,
  relevantOnly: false,
  canRegister: null,
  withCompetitions: false,
  withBirthdays: false,
} satisfies Required<EventSearchConditionInput>;

function conditionKey(condition: EventSearchConditionInput) {
  return JSON.stringify(Object.entries(conditionDefaults).map(([field, fallback]) => {
    const key = field as keyof EventSearchConditionInput;
    const value = key === 'relevantOnly' && condition.onlyMine
      ? false : condition[key] ?? fallback;
    return Array.isArray(value) ? [...new Set(value)].toSorted() : value;
  }));
}

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
  const rows = standardViews.map((view) => ({
    ...view,
    search: savedSearches.find((search) =>
      conditionKey(search.condition) === conditionKey(view.condition)),
  }));
  const custom = savedSearches.filter((search) => !rows.some((row) =>
    conditionKey(search.condition) === conditionKey(row.condition)));
  const views = [
    ...rows,
    ...custom.map((search) => ({ ...search, name: savedSearchName(search.name), search })),
  ];

  return (
    <>
      <DropdownMenu open={open} onOpenChange={setOpen}>
        <DropdownMenuTrigger className={buttonCls({ variant: 'outline', size: 'sm', className: 'mb-2 self-start' })}>
          Uložené filtry{selected ? `: ${savedSearchName(selected.name)}` : ''} <ChevronDown />
        </DropdownMenuTrigger>
        <DropdownMenuContent
          align="start"
          onCloseAutoFocus={(event) => {
            if (editingId !== undefined) event.preventDefault();
          }}
        >
          {views.map((view) => {
            const active = value === view.id || (selected != null &&
              conditionKey(selected.condition) === conditionKey(view.condition));
            return (
              <DropdownMenuButton
                key={view.id ?? 'default'}
                textValue={view.name}
                onSelect={() => onChange(view.search?.id ?? view.id)}
                className="py-0 pr-0"
              >
                <Check className={active ? 'size-4 shrink-0' : 'size-4 shrink-0 invisible'} />
                <span className="grow py-3">{view.name}</span>
                {view.search && (
                  <button
                    type="button"
                    title="Upravit filtr"
                    className="self-stretch rounded-r-[3px] border-l border-neutral-4 px-3 hover:bg-accent-5 hover:text-accent-12 focus-visible:outline-2 focus-visible:outline-accent-9"
                    onPointerUp={(event) => event.stopPropagation()}
                    onClick={(event) => {
                      event.stopPropagation();
                      setEditingId(view.search!.id);
                      setOpen(false);
                    }}
                  >
                    <Settings className="size-4" />
                  </button>
                )}
              </DropdownMenuButton>
            );
          })}
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
          <DialogContent onOpenAutoFocus={(event) => event.preventDefault()}>
            <DialogTitle>{editing ? 'Upravit uložený filtr' : 'Nový uložený filtr'}</DialogTitle>
            <EventSavedSearchForm savedSearch={editing} />
          </DialogContent>
        </Dialog>
      )}
    </>
  );
}
