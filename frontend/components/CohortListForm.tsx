import * as Collapsible from '@radix-ui/react-collapsible';
import { useQueryClient } from '@tanstack/react-query';
import classNames from 'classnames';
import { Command } from 'cmdk';
import { CohortGroupDocument, CohortGroupFragment } from 'lib/graphql/CohortGroup';
import { CohortListDocument, UpdateCohortDocument } from 'lib/graphql/Cohorts';
import { getGqlKey, useGqlMutation, useGqlQuery } from 'lib/query';
import React from 'react';
import { ChevronDown, Plus, PlusCircle, Search, Trash2, Triangle } from 'react-feather';

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
        <Collapsible.Root className="border flex flex-col gap-2 mb-1 pl-2 rounded-lg">
          <Collapsible.Trigger className="group flex justify-between p-2">
            {x.sName}
            <ChevronDown className="text-stone-400 transform duration-300 ease-in-out group-radix-state-open:rotate-180" />
          </Collapsible.Trigger>
          <Collapsible.Content className="CollapsibleContent p-2">
            <button
              className="button button-white gap-2"
              onClick={() => update({ id: x.id, patch: { cohortGroup: null } })}
            >
              <Trash2 /> Odstranit
            </button>
          </Collapsible.Content>
        </Collapsible.Root>
      ))}

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
            placeholder="Přidat skupinu..."
            className={classNames(
              'placeholder:text-muted flex h-10 pl-10 w-full border-none rounded-md bg-transparent py-2 text-sm outline-none',
              'disabled:cursor-not-allowed disabled:opacity-50 focus:ring-red-500',
            )}
          />
          <Plus className="absolute right-3 top-[.9rem] h-4 w-4 shrink-0 opacity-50" />
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
    </div>
  );
}
