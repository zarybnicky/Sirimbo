import { CalendarFeedSubscriptionsDocument, CalendarFeedSubscriptionFieldsFragment, CreateCalendarFeedSubscriptionDocument, DeleteCalendarFeedSubscriptionDocument, UpdateCalendarFeedSubscriptionDocument } from '@/graphql/CalendarFeedSubscription';
import { EventType } from '@/graphql';
import { origin } from '@/graphql/query';
import { Dialog, DialogContent, DialogDescription, DialogHeader, DialogTitle, DialogTrigger } from '@/ui/dialog';
import { formatEventType } from '@/ui/format';
import { Spinner } from '@/ui/Spinner';
import { buttonCls } from '@/ui/style';
import { useConfirm } from '@/ui/Confirm';
import { TextField } from '@/ui/fields/text';
import { Checkbox } from '@/ui/fields/checkbox';
import { Combobox } from '@/ui/fields/Combobox';
import { CalendarIcon, CopyIcon, FilePlus2Icon, PencilIcon, Trash2Icon } from 'lucide-react';
import React from 'react';
import { toast } from 'react-toastify';
import { useMutation, useQuery } from 'urql';

const eventTypeOptions: EventType[] = ['CAMP', 'GROUP', 'HOLIDAY', 'LESSON', 'RESERVATION'];

const defaultFormState = {
  onlyType: '',
  onlyMine: false,
  startOffsetDays: '-7',
  endOffsetDays: '90',
};

type FormState = typeof defaultFormState;

const toNumber = (value: string, fallback: number): number => {
  const parsed = Number.parseInt(value, 10);
  return Number.isNaN(parsed) ? fallback : parsed;
};

const feedSummary = (feed: CalendarFeedSubscriptionFieldsFragment) => {
  const typeLabel = feed.onlyType ? formatEventType({ type: feed.onlyType }) : 'všechny typy';
  const scope = feed.onlyMine ? 'Pouze moje události' : 'Všechny události';
  const start = feed.startOffsetDays;
  const end = feed.endOffsetDays;
  const range = end === null ? `od ${start} dnů od dneška` : `od ${start} do ${end} dnů od dneška`;
  return `${scope} • ${typeLabel} • ${range}`;
};

const buildFeedUrl = (token: string) => `${origin}/calendar/feeds/${token}.ics`;

export function CalendarFeedDialog() {
  const confirm = useConfirm();
  const [open, setOpen] = React.useState(false);
  const [form, setForm] = React.useState<FormState>(defaultFormState);
  const [editingId, setEditingId] = React.useState<string | null>(null);

  const [{ data, fetching }, reexecuteQuery] = useQuery({
    query: CalendarFeedSubscriptionsDocument,
    pause: !open,
  });

  const [createResult, createFeed] = useMutation(CreateCalendarFeedSubscriptionDocument);
  const [updateResult, updateFeed] = useMutation(UpdateCalendarFeedSubscriptionDocument);
  const [deleteResult, deleteFeed] = useMutation(DeleteCalendarFeedSubscriptionDocument);

  const feeds = data?.calendarFeedSubscriptionsList ?? [];
  const saving = createResult.fetching || updateResult.fetching;
  const eventTypeItems = React.useMemo(
    () => eventTypeOptions.map((type) => ({ id: type, label: formatEventType({ type }) })),
    [],
  );

  React.useEffect(() => {
    if (!open) {
      setForm(defaultFormState);
      setEditingId(null);
      return;
    }
    reexecuteQuery({ requestPolicy: 'network-only' });
  }, [open, reexecuteQuery]);

  React.useEffect(() => {
    if (!editingId) {
      setForm(defaultFormState);
      return;
    }
    const feed = feeds.find((item) => item.id === editingId);
    if (!feed) {
      setForm(defaultFormState);
      setEditingId(null);
      return;
    }
    setForm({
      onlyType: feed.onlyType ?? '',
      onlyMine: feed.onlyMine,
      startOffsetDays: feed.startOffsetDays.toString(),
      endOffsetDays: feed.endOffsetDays === null ? '' : feed.endOffsetDays.toString(),
    });
  }, [editingId, feeds]);

  const handleSubmit = async (event: React.FormEvent<HTMLFormElement>) => {
    event.preventDefault();
    const input = {
      onlyType: form.onlyType ? (form.onlyType as EventType) : null,
      onlyMine: form.onlyMine,
      startOffsetDays: toNumber(form.startOffsetDays, 0),
      endOffsetDays: form.endOffsetDays === '' ? null : toNumber(form.endOffsetDays, 0),
    };

    if (editingId) {
      const result = await updateFeed({ id: editingId, patch: input });
      if (result.error) {
        toast.error('Uložení se nezdařilo.');
        return;
      }
      toast.success('Feed byl upraven.');
    } else {
      const result = await createFeed({ input });
      if (result.error) {
        toast.error('Vytvoření se nezdařilo.');
        return;
      }
      const newId = result.data?.createCalendarFeedSubscription?.calendarFeedSubscription?.id;
      if (newId) {
        setEditingId(newId);
      }
      toast.success('Feed byl vytvořen.');
    }

    reexecuteQuery({ requestPolicy: 'network-only' });
  };

  const handleDelete = async (id: string) => {
    try {
      await confirm({
        title: 'Smazat kalendář?',
        description: 'Odkaz přestane fungovat a vnější kalendáře ho přestanou načítat.',
        confirmationText: 'Smazat',
      });
    } catch {
      return;
    }

    const result = await deleteFeed({ id });
    if (result.error) {
      toast.error('Smazání se nezdařilo.');
      return;
    }

    toast.success('Feed byl smazán.');
    if (editingId === id) {
      setEditingId(null);
      setForm(defaultFormState);
    }
    reexecuteQuery({ requestPolicy: 'network-only' });
  };

  const copyLink = async (feed: CalendarFeedSubscriptionFieldsFragment) => {
    const url = buildFeedUrl(feed.token);
    try {
      await navigator.clipboard.writeText(url);
      toast.success('Odkaz zkopírován do schránky.');
    } catch (error) {
      console.error('Copy failed', error);
      toast.error('Nepodařilo se zkopírovat odkaz.');
    }
  };

  const startLabel = 'Počet dnů od dneška pro začátek (záporné číslo zobrazí minulost).';
  const endLabel = 'Počet dnů od dneška pro konec (ponechte prázdné pro neomezený konec).';

  return (
    <Dialog open={open} onOpenChange={setOpen}>
      <DialogTrigger
        variant="outline"
        text={(
          <>
            <CalendarIcon className="size-4" />
            iCal export
          </>
        )}
      />
      <DialogContent className="sm:max-w-2xl">
        <DialogHeader>
          <DialogTitle>iCal kalendář</DialogTitle>
          <DialogDescription>
            Vytvořte si odkaz pro předplatné v Apple, Google či Outlook kalendáři. Odkaz lze
            poskytnout jen členům, kterým věříte.
          </DialogDescription>
        </DialogHeader>

        {fetching ? (
          <div className="flex justify-center py-10">
            <Spinner />
          </div>
        ) : (
          <div className="grid gap-6">
            <form className="grid gap-3" onSubmit={handleSubmit}>
              <fieldset className="grid gap-3" disabled={saving}>
                <Combobox
                  value={form.onlyType || null}
                  onChange={(value) =>
                    setForm((prev) => ({
                      ...prev,
                      onlyType: (value ?? '') as FormState['onlyType'],
                    }))
                  }
                  options={eventTypeItems}
                  label="Typ akce"
                  placeholder="Všechny typy"
                />

                <Checkbox
                  name="onlyMine"
                  label="Pouze moje události"
                  checked={form.onlyMine}
                  onChange={(event) =>
                    setForm((prev) => ({ ...prev, onlyMine: event.currentTarget.checked }))
                  }
                />

                <TextField
                  name="startOffsetDays"
                  type="number"
                  label="Začátek období (dny)"
                  value={form.startOffsetDays}
                  onChange={(event) =>
                    setForm((prev) => ({ ...prev, startOffsetDays: event.currentTarget.value }))
                  }
                  helperText={startLabel}
                />

                <TextField
                  name="endOffsetDays"
                  type="number"
                  label="Konec období (dny)"
                  value={form.endOffsetDays}
                  onChange={(event) =>
                    setForm((prev) => ({ ...prev, endOffsetDays: event.currentTarget.value }))
                  }
                  placeholder="bez omezení"
                  helperText={endLabel}
                />
              </fieldset>

              <div className="flex flex-wrap gap-2 justify-end">
                {editingId && (
                  <button
                    type="button"
                    className={buttonCls({ variant: 'outline', size: 'sm' })}
                    onClick={() => handleDelete(editingId)}
                    disabled={deleteResult.fetching}
                  >
                    <Trash2Icon className="size-4" />
                    Smazat feed
                  </button>
                )}
                <button type="submit" className={buttonCls({ size: 'sm' })} disabled={saving}>
                  {editingId ? (
                    <>
                      <PencilIcon className="size-4" />
                      Uložit změny
                    </>
                  ) : (
                    <>
                      <FilePlus2Icon className="size-4" />
                      Vytvořit feed
                    </>
                  )}
                </button>
              </div>
            </form>

            <div className="grid gap-3">
              <div className="flex items-center justify-between">
                <h3 className="text-base font-semibold">Existující odkazy</h3>
                <button
                  type="button"
                  className={buttonCls({ variant: 'outline', size: 'sm' })}
                  onClick={() => {
                    setEditingId(null);
                    setForm(defaultFormState);
                  }}
                >
                  <FilePlus2Icon className="size-4" />
                  Nový odkaz
                </button>
              </div>

              {feeds.length === 0 ? (
                <p className="text-sm text-neutral-11">
                  Zatím nemáte žádný uložený iCal feed. Vytvořte ho pomocí formuláře výše.
                </p>
              ) : (
                <ul className="grid gap-3">
                  {feeds.map((feed) => {
                    const url = buildFeedUrl(feed.token);
                    return (
                      <li
                        key={feed.id}
                        className="rounded-xl border border-neutral-6 bg-neutral-1 p-3 shadow-sm"
                      >
                        <div className="flex flex-wrap justify-between gap-3">
                          <div>
                            <div className="font-medium text-neutral-12">
                              {feedSummary(feed)}
                            </div>
                            <div className="text-xs text-neutral-10 break-all mt-1">
                              {url}
                            </div>
                          </div>
                          <div className="flex gap-2">
                            <button
                              type="button"
                              className={buttonCls({ variant: 'outline', size: 'sm' })}
                              onClick={() => void copyLink(feed)}
                            >
                              <CopyIcon className="size-4" />
                              Zkopírovat
                            </button>
                            <button
                              type="button"
                              className={buttonCls({ variant: editingId === feed.id ? 'primary' : 'outline', size: 'sm' })}
                              onClick={() => setEditingId(feed.id)}
                            >
                              <PencilIcon className="size-4" />
                              Upravit
                            </button>
                          </div>
                        </div>
                      </li>
                    );
                  })}
                </ul>
              )}
            </div>
          </div>
        )}
      </DialogContent>
    </Dialog>
  );
}

export function CalendarFeedDialogButton() {
  return <CalendarFeedDialog />;
}
