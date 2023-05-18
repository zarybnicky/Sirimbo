import { useQueryClient } from '@tanstack/react-query';
import classNames from 'classnames';
import { Command } from 'cmdk';
import { CohortGroupDocument, CohortGroupFragment } from 'lib/graphql/CohortGroup';
import { CohortListDocument, UpdateCohortDocument } from 'lib/graphql/Cohorts';
import { getGqlKey, useGqlMutation, useGqlQuery } from 'lib/query';
import React from 'react';
import { ChevronDown, Plus, Search, Trash2 } from 'react-feather';
import { CollapsibleCard } from './CollapsibleCard';

type Props = {
  data: CohortGroupFragment;
};

export function CohortListForm({ data }: Props) {
  const queryClient = useQueryClient();
  const { data: cohorts } = useGqlQuery(CohortListDocument, {});
  const remaining = React.useMemo(() => {
    const used = data.skupiniesByCohortGroup.nodes.map((x) => x.id);
    return (cohorts?.skupinies?.nodes || []).filter((x) => !used.includes(x.id));
  }, [cohorts, data]);

  const { mutateAsync: update } = useGqlMutation(UpdateCohortDocument, {
    onSuccess() {
      queryClient.invalidateQueries(getGqlKey(CohortGroupDocument, { id: data.id }));
    },
  });

  const [toAdd, setToAdd] = React.useState<string | undefined>();

  return (
    <div className="py-2 mt-4 mb-2">
      <div className="col-span-2 text-stone-700 text-sm pb-1">
        Tréninkové skupiny v programu
      </div>
      {data.skupiniesByCohortGroup.nodes.map((x) => (
        <CollapsibleCard key={x.id} title={x.sName} cohort={x}>
          <button
            className="button button-white gap-2"
            onClick={() => update({ id: x.id, patch: { cohortGroup: null } })}
          >
            <Trash2 /> Odstranit
          </button>
        </CollapsibleCard>
      ))}

      <CollapsibleCard
        title={
          <span>
            <Plus className="inline w-4 h-4 text-stone-600" /> Přidat skupinu
          </span>
        }
      >
        <Command
          value={toAdd}
          onValueChange={setToAdd}
          className={classNames(
            'border rounded-lg',
            '[&_[cmdk-group-heading]]:px-2 [&_[cmdk-group-heading]]:font-medium [&_[cmdk-group-heading]]:text-muted',
            '[&_[cmdk-group]:not([hidden])_~[cmdk-group]]:pt-0 [&_[cmdk-group]]:px-2',
            '[&_[cmdk-input-wrapper]_svg]:h-5 [&_[cmdk-input-wrapper]_svg]:w-5',
            '[&_[cmdk-input]]:h-12',
            '[&_[cmdk-item]]:px-2 [&_[cmdk-item]]:py-3 [&_[cmdk-item]_svg]:h-5 [&_[cmdk-item]_svg]:w-5',
          )}
        >
          <div className="relative border-b" cmdk-input-wrapper="">
            <Search className="absolute left-3 top-[.9rem] h-4 w-4 shrink-0 opacity-50" />
            <Command.Input
              autoFocus
              placeholder="Vyhledat.."
              className={classNames(
                'placeholder:text-muted flex h-10 pl-10 w-full border-none bg-transparent py-2 text-sm outline-none',
                'disabled:cursor-not-allowed disabled:opacity-50 focus:ring-transparent',
              )}
            />
          </div>

          <Command.List className="h-[300px] overflow-y-auto scrollbar">
            {remaining.map((x) => (
              <Command.Item
                key={x.id}
                value={x.sName}
                onSelect={() => {
                  update({ id: x.id, patch: { cohortGroup: data.id } });
                  setToAdd(undefined);
                }}
                className={classNames(
                  'relative flex cursor-default select-none items-center rounded-sm px-2 py-1.5 text-sm outline-none aria-selected:bg-red-500 aria-selected:text-white',
                  'data-[disabled]:pointer-events-none data-[disabled]:opacity-50',
                )}
              >
                {x.sName}
              </Command.Item>
            ))}
          </Command.List>
        </Command>
      </CollapsibleCard>
    </div>
  );
}
