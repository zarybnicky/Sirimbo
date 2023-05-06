import { useQueryClient } from '@tanstack/react-query';
import { CohortGroupFragment, useCohortGroupQuery } from 'lib/graphql/CohortGroup';
import { useCohortListQuery, useUpdateCohortMutation } from 'lib/graphql/Cohorts';
import React from 'react';
import { PlusCircle, Trash2 } from 'react-feather';
import { useForm } from 'react-hook-form';
import { SelectElement } from './SelectElement';

type Props = {
  data: CohortGroupFragment;
};

export function CohortListForm({ data }: Props) {
  const queryClient = useQueryClient();
  const { data: cohorts } = useCohortListQuery();
  const remaining = React.useMemo(() => {
    const used = data.skupiniesByCohortGroup.nodes.map((x) => x.id);
    const others = (cohorts?.skupinies?.nodes || []).filter((x) => !used.includes(x.id));
    return others.map((x) => ({ id: x.id, label: x.sName }));
  }, [cohorts, data]);

  const { mutateAsync: update } = useUpdateCohortMutation({
    onSuccess() {
      queryClient.invalidateQueries(useCohortGroupQuery.getKey({ id: data.id }));
    },
  });

  const { control, watch, getValues, setValue } = useForm();

  return (
    <div className="grid grid-cols-[2fr_1fr] gap-2 py-2 my-2">
      <div className="col-span-2 text-stone-700 text-sm ">Tréninkové skupiny v programu</div>
      {data.skupiniesByCohortGroup.nodes.map((x) => (
        <>
          <div>{x.sName}</div>
          <button className="text-red-700 w-4" onClick={() => update({ id: x.id, patch: { cohortGroup: null } })}>
            <Trash2 />
          </button>
        </>
      ))}
      <>
        <SelectElement control={control} name="id" options={remaining} />
        <button
          className="text-red-700 disabled:text-red-700/70 w-4"
          disabled={!watch('id')}
          onClick={() => {
            update({ id: getValues('id'), patch: { cohortGroup: data.id } });
            setValue('id', '');
          }}
        >
          <PlusCircle />
        </button>
      </>
    </div>
  );
}
