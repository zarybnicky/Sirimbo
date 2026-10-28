import { PersonMembershipsDocument } from '@/graphql/Person';
import { DialogTitle } from '@/ui/dialog';
import { CohortListDocument, SyncCohortMembershipsDocument } from '@/graphql/Cohorts';
import { FieldHelper, FormError, useFormResult } from '@/ui/form';
import { ComboboxSearchArea } from '@/ui/fields/Combobox';
import { SubmitButton } from '@/ui/submit';
import { buttonCls } from '@/ui/style';
import { X } from 'lucide-react';
import { useMutation, useQuery } from 'urql';
import { useController, useForm } from 'react-hook-form';

export function AddToCohortForm({ personId }: { personId: string }) {
  const { onSuccess } = useFormResult();
  const { control, handleSubmit, setError } = useForm({
    defaultValues: { cohortIds: [] as string[] },
  });
  const { field, fieldState } = useController({
    control,
    name: 'cohortIds',
    rules: { validate: (ids) => ids.length > 0 || 'Vyberte alespoň jednu skupinu' },
  });
  const selectedIds = field.value;
  const [result, syncCohorts] = useMutation(SyncCohortMembershipsDocument);
  const [query] = useQuery({
    query: PersonMembershipsDocument,
    variables: { id: personId },
  });
  const [cohortQuery] = useQuery({
    query: CohortListDocument,
    variables: { archived: false },
  });
  const queryError = query.error || cohortQuery.error;
  const person = query.data?.person;
  const memberships =
    person?.cohortMembershipsList.filter((x) => x.status !== 'EXPIRED') ?? [];
  const existingIds = new Set(memberships.map((x) => x.cohortId));
  const cohorts = cohortQuery.data?.cohortsList ?? [];
  const toAdd = cohorts.filter(
    (x) => selectedIds.includes(x.id) && !existingIds.has(x.id),
  );
  const options = cohorts
    .filter((x) => !existingIds.has(x.id) && !selectedIds.includes(x.id))
    .map((x) => ({ id: x.id, label: x.name }));

  const rows = [
    ...memberships,
    ...toAdd.map((cohort) => ({
      id: `new:${cohort.id}`,
      cohortId: cohort.id,
      cohort,
      status: 'NEW' as const,
    })),
  ];

  const onSubmit = async () => {
    const cohortIds = rows.filter((x) => x.status !== 'PENDING').map((x) => x.cohortId);
    const result = await syncCohorts({
      input: { personId, cohortIds: [...new Set(cohortIds)] },
    });
    if (result.error) {
      setError('root.server', { message: result.error.message });
      return;
    }
    onSuccess();
  };

  return (
    <form className="space-y-3" onSubmit={handleSubmit(onSubmit)}>
      <DialogTitle>Přidat do skupiny</DialogTitle>
      <FormError error={result.error || queryError} />
      {query.fetching && !person && <p>Načítám skupiny…</p>}
      {!query.fetching && !query.error && !person && <p>Osoba nebyla nalezena.</p>}
      {person && (
        <>
          {rows.length > 0 && (
            <ul className="divide-y divide-neutral-4 rounded-md border border-neutral-4">
              {rows.map(({ id, cohortId, cohort, status }) => (
                <li
                  key={id}
                  className={`flex items-center justify-between gap-3 px-3 py-2 text-sm ${status === 'NEW' ? 'bg-accent-2' : ''}`}
                >
                  <span>{cohort?.name ?? cohortId}</span>
                  <div className="flex shrink-0 items-center gap-2">
                    <span
                      className={status === 'NEW' ? 'text-accent-11' : 'text-neutral-11'}
                    >
                      {status === 'NEW'
                        ? 'Nová'
                        : status === 'PENDING'
                          ? 'Plánováná'
                          : ''}
                    </span>
                    {status === 'NEW' && (
                      <button
                        type="button"
                        className={buttonCls({ variant: 'outline', size: 'xs' })}
                        onClick={() =>
                          field.onChange(selectedIds.filter((id) => id !== cohortId))
                        }
                      >
                        <X className="size-4" />
                      </button>
                    )}
                  </div>
                </li>
              ))}
            </ul>
          )}
          {options.length > 0 ? (
            <div>
              <ComboboxSearchArea
                inline
                options={options}
                onChange={(id) => {
                  if (id) field.onChange([...selectedIds, id]);
                }}
              />
            </div>
          ) : (
            <p className="text-sm text-neutral-11">Žádné další skupiny k přidání.</p>
          )}
          <FieldHelper error={fieldState.error} />
        </>
      )}
      <SubmitButton
        control={control}
        disabled={!person || !!queryError || query.fetching || cohortQuery.fetching || toAdd.length === 0}
      >
        Uložit změny
      </SubmitButton>
    </form>
  );
}
