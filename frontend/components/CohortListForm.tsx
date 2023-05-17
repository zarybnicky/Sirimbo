import { useQueryClient } from '@tanstack/react-query';
import { CohortGroupDocument, CohortGroupFragment } from 'lib/graphql/CohortGroup';
import { CohortListDocument, UpdateCohortDocument } from 'lib/graphql/Cohorts';
import { getGqlKey, useGqlMutation, useGqlQuery } from 'lib/query';
import React from 'react';
import { PlusCircle, Trash2 } from 'react-feather';
import { useForm } from 'react-hook-form';
import { SelectElement } from './SelectElement';

type Props = {
  data: CohortGroupFragment;
};

export function CohortListForm({ data }: Props) {
  const queryClient = useQueryClient();
  const { data: cohorts } = useGqlQuery(CohortListDocument, {});
  const remaining = React.useMemo(() => {
    const used = data.skupiniesByCohortGroup.nodes.map((x) => x.id);
    const others = (cohorts?.skupinies?.nodes || []).filter((x) => !used.includes(x.id));
    return others.map((x) => ({ id: x.id, label: x.sName }));
  }, [cohorts, data]);

  const { mutateAsync: update } = useGqlMutation(UpdateCohortDocument, {
    onSuccess() {
      queryClient.invalidateQueries(getGqlKey(CohortGroupDocument, { id: data.id }));
    },
  });

  const { control, watch, getValues, setValue } = useForm();

  return (
    <div className="py-2 mt-4 mb-2">
      <div className="col-span-2 text-stone-700 text-sm pb-1">
        Tréninkové skupiny v programu
      </div>
      <div className="grid grid-cols-[7fr_1fr] gap-2 border rounded-lg w-fit">
        {data.skupiniesByCohortGroup.nodes.map((x) => (
          <>
            <div className="p-2">{x.sName}</div>
            <button
              className="text-red-500"
              onClick={() => update({ id: x.id, patch: { cohortGroup: null } })}
            >
              <Trash2 />
            </button>
          </>
        ))}
        <SelectElement
          className="min-w-[250px]"
          control={control}
          name="id"
          options={remaining}
        />
        <button
          className="text-red-500 disabled:text-red-200 pr-2"
          disabled={!watch('id')}
          onClick={() => {
            update({ id: getValues('id'), patch: { cohortGroup: data.id } });
            setValue('id', '');
          }}
        >
          <PlusCircle />
        </button>
      </div>
    </div>
  );
}
