import {
  CohortGroupDocument,
  CreateCohortGroupDocument,
  DeleteCohortGroupDocument,
  UpdateCohortGroupDocument,
} from '@/graphql/CohortGroup';
import { UpdateCohortDocument } from '@/graphql/Cohorts';
import { Card, CardMenu } from '@/ui/Card';
import { useConfirm } from '@/ui/Confirm';
import { DeleteButton } from '@/ui/DeleteButton';
import { ErrorPage } from '@/ui/ErrorPage';
import { TitleBar } from '@/ui/TitleBar';
import { cn } from '@/ui/cn';
import { Command, CommandInput, CommandItem, CommandList } from '@/ui/command';
import { DropdownMenu, DropdownMenuButton, DropdownMenuContent, DropdownMenuTrigger } from '@/ui/dropdown';
import { CheckboxElement } from '@/ui/fields/checkbox';
import { RichTextEditor } from '@/ui/fields/richtext';
import { TextFieldElement } from '@/ui/fields/text';
import { FormError } from '@/ui/form';
import { buttonCls } from '@/ui/style';
import { SubmitButton } from '@/ui/submit';
import { useCohorts } from '@/ui/useCohorts';
import { zodResolver } from '@hookform/resolvers/zod';
import * as Popover from '@radix-ui/react-popover';
import { Plus } from 'lucide-react';
import { useRouter } from 'next/router';
import React from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { useForm } from 'react-hook-form';
import { toast } from 'react-toastify';
import { useMutation, useQuery } from 'urql';
import { z } from 'zod';

const Form = z.object({
  name: z.string(),
  description: z.string().default(''),
  isPublic: z.boolean().default(false),
  ordering: z.number().default(0),
});
type FormProps = z.infer<typeof Form>;

export function CohortGroupForm({ id = '' }: { id?: string }) {
  const router = useRouter();
  const confirm = useConfirm();
  const [query] = useQuery({query: CohortGroupDocument, variables: { id }, pause: !id });
  const data = query.data?.cohortGroup;
  const title = data ? data.name || '(Bez názvu)' : 'Nový tréninkový program';

  const { data: allCohorts } = useCohorts();

  const create = useMutation(CreateCohortGroupDocument)[1];
  const update = useMutation(UpdateCohortGroupDocument)[1];
  const updateCohort = useMutation(UpdateCohortDocument)[1];
  const deleteMutation = useMutation(DeleteCohortGroupDocument)[1];

  const remaining = React.useMemo(() => {
    const used = (data?.cohortsList || []).map((x) => x.id);
    return allCohorts.filter((x) => !used.includes(x.id));
  }, [allCohorts, data]);

  const { reset, control, handleSubmit } = useForm<FormProps>({
    resolver: zodResolver(Form),
  });
  React.useEffect(() => {
    reset(Form.partial().optional().parse(data));
  }, [data, reset]);

  const onSubmit = useAsyncCallback(async (patch: FormProps) => {
    if (id) {
      await update({ id, patch });
    } else {
      const res = await create({ input: patch });
      const id = res.data!.createCohortGroup?.cohortGroup?.id;
      toast.success('Přidáno.');
      if (id) {
        router.replace(`/treninkove-programy/${id}`);
      } else {
        reset(undefined);
      }
    }
  });

  if (query.data && query.data.cohortGroup === null) {
    return <ErrorPage error="Nenalezeno" />;
  }

  return (
    <form className="container space-y-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <TitleBar title={title}>
        {data && (
          <DropdownMenu>
            <DropdownMenuTrigger.CornerDots />
            <DropdownMenuContent align="end">
              <DropdownMenuButton
                onClick={async () => {
                  await confirm({ description: `Opravdu chcete smazat tréninkový program "${data.name}"?` });
                  await deleteMutation({ id })
                  router.replace('/treninkove-programy')
                }}
              >
                Smazat
              </DropdownMenuButton>
            </DropdownMenuContent>
          </DropdownMenu>
        )}
        {data && (
          <DeleteButton
            doc={DeleteCohortGroupDocument}
            id={id}
            title="smazat tréninkový program"
            redirect="/treninkove-programy"
          />
        )}
        <SubmitButton loading={onSubmit.loading} />
      </TitleBar>

      <FormError error={onSubmit.error} />
      <TextFieldElement control={control} name="name" label="Název" required />
      <CheckboxElement control={control} name="isPublic" label="Zobrazit pro veřejnost" />
      <TextFieldElement
        control={control}
        type="number"
        name="ordering"
        label="Pořadí v seznamech skupin (1 = první, 999 = poslední)"
      />
      <RichTextEditor
        control={control}
        initialState={data?.description}
        name="description"
        label="Popis"
      />

      {id && (
        <div className="mt-1 pb-8">
          <div className="text-stone-700 text-sm pb-1">Tréninkové skupiny v programu</div>

          {data?.cohortsList.map((x) => (
            <Card key={x.id} cohort={x}>
              <CardMenu>
                <DropdownMenuButton onClick={() => updateCohort({ id: x.id, patch: { cohortGroupId: null } })}>
                  Odebrat
                </DropdownMenuButton>
              </CardMenu>
              {x.name}
            </Card>
          ))}

          <Popover.Root>
            <Popover.Trigger asChild>
              <button className={buttonCls({ variant: 'outline' })}>
                <Plus /> Přidat skupinu
              </button>
            </Popover.Trigger>
            <Popover.Content
              align="start"
              sideOffset={4}
              className={cn(
                'z-30 data-[side=top]:animate-slideUpAndFade data-[side=bottom]:animate-slideDownAndFade',
                'w-48 rounded-lg shadow-md md:w-56 bg-neutral-1',
              )}
            >
              <Popover.Arrow className="fill-current text-white" />
              <Command className="border">
                <CommandInput autoFocus placeholder="Vyhledat..." />
                <CommandList>
                  {remaining.map((x) => (
                    <CommandItem
                      key={x.id}
                      value={x.name}
                      onSelect={() => updateCohort({ id: x.id, patch: { cohortGroupId: id } })}
                    >
                      {x.name}
                    </CommandItem>
                  ))}
                </CommandList>
              </Command>
            </Popover.Content>
          </Popover.Root>
        </div>
      )}
    </form>
  );
}
