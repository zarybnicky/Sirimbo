"use client"

import * as React from "react"
import {
  ColumnDef,
  flexRender,
  getCoreRowModel,
  getFilteredRowModel,
  getSortedRowModel,
  getExpandedRowModel,
  getPaginationRowModel,
  useReactTable,
  type SortingState,
  type ColumnFiltersState,
  type VisibilityState,
  type RowSelectionState,
  type ExpandedState,
  type PaginationState,
  type Table as TableType,
  type Row,
  OnChangeFn,
  HeaderContext,
  CellContext,
} from "@tanstack/react-table"
import { useVirtualizer } from "@tanstack/react-virtual"
import { rankItem } from "@tanstack/match-sorter-utils"
import * as DropdownMenu from "@radix-ui/react-dropdown-menu"
import * as Checkbox from "@radix-ui/react-checkbox"
import { CheckIcon } from "lucide-react";

/**
 * Minimal-changes DataTable supporting:
 * - optional external state (controlled) with internal-state fallback
 * - sidebar vs full presets via initialColumnVisibility + estimatedRowHeight
 * - pluggable scroll container via scrollParentRef (attached to our container)
 * - toolbar & selected-actions render slots
 * - optional row click navigation
 * - virtualization with padding rows (no invalid <div> in <table>)
 * - Radix Colors-friendly classnames (bg-neutral-*, text-neutral-*, primary-*)
 */

// Fuzzy filter using TanStack match-sorter utils
const fuzzyFilter = (row: any, columnId: string, value: string, addMeta: any) => {
  const itemRank = rankItem(row.getValue(columnId) ?? "", value ?? "")
  addMeta({ itemRank })
  return itemRank.passed
}

export type DataTableState = {
  sorting?: SortingState
  columnFilters?: ColumnFiltersState
  globalFilter?: string
  columnVisibility?: VisibilityState
  rowSelection?: RowSelectionState
  expanded?: ExpandedState
  pagination?: PaginationState
}

export type DataTableProps<TData, TValue> = {
  data: TData[]
  columns: ColumnDef<TData, TValue>[]
  /**
   * Optional external/controlled state. Provide any subset; others fall back to internal state.
   * For sidebar vs full, prefer setting `initialColumnVisibility` and differing `estimatedRowHeight`.
   */
  state?: DataTableState
  // granular external change handlers (fallbacks to internal setState when omitted)
  onSortingChange?: OnChangeFn<SortingState>;
  onColumnFiltersChange?: OnChangeFn<ColumnFiltersState>;
  onGlobalFilterChange?: OnChangeFn<string>;
  onColumnVisibilityChange?: OnChangeFn<VisibilityState>;
  onRowSelectionChange?: OnChangeFn<RowSelectionState>;
  onExpandedChange?: OnChangeFn<ExpandedState>;
  onPaginationChange?: OnChangeFn<PaginationState>;

  /** initial state seeds for internal mode */
  initialColumnVisibility?: VisibilityState
  initialGlobalFilter?: string

  /** layout */
  estimatedRowHeight?: number // px; override for compact sidebar
  scrollParentRef?: React.RefObject<HTMLElement> // attached to our scroll container

  /** slots */
  toolbar?: (ctx: { table: TableType<TData> }) => React.ReactNode
  selectedActions?: (ctx: { selected: TData[]; table: TableType<TData> }) => React.ReactNode

  /** navigation */
  onRowClick?: (row: Row<TData>) => void;
  renderExpanded?: (row: TData) => React.ReactNode;

  /** feature toggles */
  enableSelection?: boolean;
  enablePagination?: boolean;
}

export function DataTable<TData, TValue>(props: DataTableProps<TData, TValue>) {
  const {
    data,
    columns,
    state: ext,
    onSortingChange: extSetSorting,
    onColumnFiltersChange: extSetColFilters,
    onGlobalFilterChange: extSetGlobal,
    onColumnVisibilityChange: extSetColVis,
    onRowSelectionChange: extSetSelection,
    onExpandedChange: extSetExpanded,
    onPaginationChange: extSetPagination,

    initialColumnVisibility,
    initialGlobalFilter,

    estimatedRowHeight = 40,
    scrollParentRef,

    toolbar,
    selectedActions,
    onRowClick,
    renderExpanded,

    enableSelection = true,
    enablePagination = true,
  } = props

  // internal fallbacks
  const [iSorting, iSetSorting] = React.useState<SortingState>(ext?.sorting ?? [])
  const [iColFilters, iSetColFilters] = React.useState<ColumnFiltersState>(ext?.columnFilters ?? [])
  const [iGlobal, iSetGlobal] = React.useState<string>(ext?.globalFilter ?? initialGlobalFilter ?? "")
  const [iColVis, iSetColVis] = React.useState<VisibilityState>(ext?.columnVisibility ?? initialColumnVisibility ?? {})
  const [iSelection, iSetSelection] = React.useState<RowSelectionState>(ext?.rowSelection ?? {})
  const [iExpanded, iSetExpanded] = React.useState<ExpandedState>(ext?.expanded ?? {})
  const [iPagination, iSetPagination] = React.useState<PaginationState>(
    ext?.pagination ?? { pageIndex: 0, pageSize: 50 }
  )

  const sorting = ext?.sorting ?? iSorting
  const columnFilters = ext?.columnFilters ?? iColFilters
  const globalFilter = ext?.globalFilter ?? iGlobal
  const columnVisibility = ext?.columnVisibility ?? iColVis
  const rowSelection = (enableSelection ? ext?.rowSelection : undefined) ?? iSelection
  const expanded = ext?.expanded ?? iExpanded
  const pagination = enablePagination ? ext?.pagination ?? iPagination : undefined

  const setSorting = extSetSorting ?? iSetSorting
  const setColFilters = extSetColFilters ?? iSetColFilters
  const setGlobal = extSetGlobal ?? iSetGlobal
  const setColVis = extSetColVis ?? iSetColVis
  const setSelection = extSetSelection ?? iSetSelection
  const setExpanded = extSetExpanded ?? iSetExpanded
  const setPagination = extSetPagination ?? iSetPagination

  const table = useReactTable({
    data,
    columns: [
      // selection column
      enableSelection && {
        id: "__select",
        size: 36,
        header: ({ table }: HeaderContext<TData, unknown>) => (
          <RadixCheckbox
            checked={table.getIsAllPageRowsSelected() || (table.getIsSomePageRowsSelected() && "indeterminate")}
            onCheckedChange={(v) => table.toggleAllPageRowsSelected(!!v)}
            ariaLabel="Select all rows"
          />
        ),
        cell: ({ row }: CellContext<TData, unknown>) => (
          <RadixCheckbox
            checked={row.getIsSelected()}
            onCheckedChange={(v) => row.toggleSelected(!!v)}
            ariaLabel="Select row"
          />
        ),
        enableSorting: false,
        enableHiding: false,
      },
      renderExpanded && {
        id: "__expand",
        header: () => null,
        cell: ({ row }: CellContext<TData, unknown>) => (
          <button
            onClick={row.getToggleExpandedHandler()}
            className="size-7 rounded-md border border-neutral-6 bg-neutral-1 text-neutral-12 transition-colors hover:border-neutral-7 hover:bg-neutral-2 focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-accent-8"
            aria-label={row.getIsExpanded() ? "Collapse" : "Expand"}
          >
            {row.getIsExpanded() ? "−" : "+"}
          </button>
        ),
        size: 36,
        enableSorting: false,
        enableHiding: false,
      },
      // user columns
      ...columns,
    ].filter(Boolean) as ColumnDef<TData, TValue>[],
    state: {
      sorting,
      columnFilters,
      globalFilter,
      columnVisibility,
      ...(enableSelection ? { rowSelection } : {}),
      expanded,
      ...(enablePagination && pagination ? { pagination } : {}),
    },
    onSortingChange: setSorting,
    onColumnFiltersChange: setColFilters,
    onGlobalFilterChange: setGlobal,
    onColumnVisibilityChange: setColVis,
    onRowSelectionChange: enableSelection ? setSelection : undefined,
    onExpandedChange: setExpanded,
    onPaginationChange: enablePagination ? setPagination : undefined,

    getCoreRowModel: getCoreRowModel(),
    getSortedRowModel: getSortedRowModel(),
    getFilteredRowModel: getFilteredRowModel(),
    getExpandedRowModel: getExpandedRowModel(),
    getPaginationRowModel: enablePagination ? getPaginationRowModel() : undefined,
    globalFilterFn: fuzzyFilter,
    filterFns: { fuzzy: fuzzyFilter },
    enableRowSelection: enableSelection,
    getRowCanExpand: React.useMemo(() => renderExpanded ? () => true : () => false, [renderExpanded]),
    debugTable: false,
  })

  // virtualization over filtered/sorted rows
  const rows = table.getRowModel().rows
  const internalScrollRef = React.useRef<HTMLDivElement>(null)
  const scrollRef = scrollParentRef ?? internalScrollRef

  const rowVirtualizer = useVirtualizer({
    count: rows.length,
    getScrollElement: () => scrollRef.current,
    estimateSize: () => estimatedRowHeight,
    overscan: 8,
  })
  const virtualItems = rowVirtualizer.getVirtualItems()
  const paddingTop = virtualItems.length > 0 ? virtualItems[0]!.start : 0
  const paddingBottom = virtualItems.length > 0
    ? rowVirtualizer.getTotalSize() - virtualItems.at(-1)!.end
    : 0

  const selected = enableSelection ? table.getFilteredSelectedRowModel().rows : []

  return (
    <div className="space-y-4">
      {/* Toolbar / Selected actions */}
      {enableSelection && selected.length > 0 ? (
        selectedActions ? (
          <div>{selectedActions({ selected: selected.map((r) => r.original), table })}</div>
        ) : (
          <div className="flex items-center gap-3 rounded-lg border border-neutral-6 bg-neutral-1 px-3 py-2 text-sm text-neutral-12 shadow-sm">
            <span className="font-medium text-neutral-11">Vybráno {selected.length}</span>
          </div>
        )
      ) : toolbar ? (
        <div>{toolbar({ table })}</div>
      ) : (
        <DefaultToolbar
          globalFilter={globalFilter}
          setGlobalFilter={setGlobal}
          table={table}
        />
      )}

      {/* Scroll container with reserved scrollbar gutter so sticky header never covers it */}
      <div
        ref={scrollRef as React.RefObject<HTMLDivElement>}
        className="max-h-[60vh] overflow-auto rounded-lg border border-neutral-6 bg-neutral-1 shadow-inner [scrollbar-gutter:stable]"
      >
        <table className="min-w-full border-collapse text-sm m-0">
          <thead className="sticky top-0 z-10 bg-neutral-2/80 backdrop-blur">
            {table.getHeaderGroups().map((hg) => (
              <tr key={hg.id} className="text-left">
                {hg.headers.map((h) => (
                  <th
                    key={h.id}
                    className="border-b border-neutral-6 p-3 text-xs font-semibold uppercase tracking-wide text-neutral-11"
                  >
                    {h.isPlaceholder ? null : (
                      <button
                        className="inline-flex items-center gap-1 text-neutral-11 transition-colors hover:text-neutral-12 focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-accent-8"
                        onClick={h.column.getToggleSortingHandler()}
                      >
                        {flexRender(h.column.columnDef.header, h.getContext())}
                        {{
                          asc: "↑",
                          desc: "↓",
                        }[h.column.getIsSorted() as "asc" | "desc"] ?? null}
                      </button>
                    )}
                  </th>
                ))}
              </tr>
            ))}
          </thead>

          <tbody className="bg-neutral-1">
            {paddingTop > 0 && (
              <tr>
                <td style={{ height: paddingTop }} />
              </tr>
            )}

            {virtualItems.map((v) => {
              const row = rows[v.index]!
              return (
                <React.Fragment key={row.id}>
                  <tr
                    data-selected={enableSelection && row.getIsSelected() ? "" : undefined}
                    className="cursor-default border-b border-neutral-6 transition-colors data-[selected]:bg-accent-3 hover:bg-neutral-2/60"
                    style={{ height: v.size }}
                    onClick={() => onRowClick?.(row)}
                  >
                    {row.getVisibleCells().map((cell) => (
                      <td
                        key={cell.id}
                        className="p-3 text-sm text-neutral-12"
                      >
                        {flexRender(cell.column.columnDef.cell, cell.getContext())}
                      </td>
                    ))}
                  </tr>

                  {row.getIsExpanded() && renderExpanded && (
                    <tr className="bg-neutral-1">
                      <td colSpan={row.getVisibleCells().length} className="p-3">
                        {renderExpanded(row.original)}
                      </td>
                    </tr>
                  )}
                </React.Fragment>
              )
            })}

            {paddingBottom > 0 && (
              <tr>
                <td style={{ height: paddingBottom }} />
              </tr>
            )}
          </tbody>
        </table>
      </div>

      {/* Footer */}
      {enablePagination && (
        <div className="flex flex-wrap items-center justify-between gap-3 rounded-lg border border-neutral-6 bg-neutral-1 px-3 py-2 text-sm text-neutral-11">
          <div className="font-medium text-neutral-12">
            Vybráno {selected.length} z {table.getFilteredRowModel().rows.length}
          </div>
          <div className="flex gap-2">
            <button
              onClick={() => table.previousPage()}
              disabled={!table.getCanPreviousPage()}
              className="rounded-lg border border-neutral-6 bg-neutral-2 px-3 py-1.5 text-sm text-neutral-12 transition-colors enabled:hover:border-neutral-7 enabled:hover:bg-neutral-3 disabled:cursor-not-allowed disabled:opacity-50 focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-accent-8"
            >
               Zpět
            </button>
            <button
              onClick={() => table.nextPage()}
              disabled={!table.getCanNextPage()}
              className="rounded-lg border border-neutral-6 bg-neutral-2 px-3 py-1.5 text-sm text-neutral-12 transition-colors enabled:hover:border-neutral-7 enabled:hover:bg-neutral-3 disabled:cursor-not-allowed disabled:opacity-50 focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-accent-8"
            >
               Další
            </button>
          </div>
        </div>
      )}
    </div>
  )
}

function DefaultToolbar<TData>({
  globalFilter,
  setGlobalFilter,
  table,
}: {
  globalFilter: string
  setGlobalFilter: (v: string) => void
  table: TableType<TData>
}) {
  return (
    <div className="flex flex-wrap items-center gap-2">
      <input
        value={globalFilter ?? ""}
        onChange={(e) => setGlobalFilter(e.target.value)}
        placeholder="Hledat…"
        className="h-9 w-64 rounded-lg border border-neutral-6 bg-neutral-1 px-3 text-sm text-neutral-12 placeholder:text-neutral-11 transition-colors focus:border-transparent focus:outline-none focus:ring-2 focus:ring-accent-8"
      />

      <DropdownMenu.Root>
        <DropdownMenu.Trigger asChild>
          <button className="h-9 rounded-lg border border-neutral-6 bg-neutral-2 px-3 text-sm font-medium text-neutral-12 transition-colors hover:border-neutral-7 hover:bg-neutral-3 focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-accent-8">
            Sloupce
          </button>
        </DropdownMenu.Trigger>
        <DropdownMenu.Content
          align="start"
          sideOffset={6}
          className="z-20 min-w-48 rounded-lg border border-neutral-6 bg-neutral-2 p-1 text-sm text-neutral-12 shadow-lg"
        >
          {table
            .getAllLeafColumns()
            .filter((c) => c.getCanHide())
            .map((col) => (
              <DropdownMenu.CheckboxItem
                key={col.id}
                checked={col.getIsVisible()}
                onCheckedChange={(v) => col.toggleVisibility(!!v)}
                className="flex cursor-pointer items-center gap-2 rounded-md px-2 py-1.5 outline-none transition-colors data-[highlighted]:bg-neutral-3"
              >
                <DropdownMenu.ItemIndicator>
                  <CheckIcon />
                </DropdownMenu.ItemIndicator>
                <span className="truncate">
                  {typeof col.columnDef.header === "string" ? col.columnDef.header : col.id}
                </span>
              </DropdownMenu.CheckboxItem>
            ))}
        </DropdownMenu.Content>
      </DropdownMenu.Root>
    </div>
  )
}

function RadixCheckbox({
  checked,
  onCheckedChange,
  ariaLabel,
}: {
  checked: boolean | "indeterminate"
  onCheckedChange: (v: boolean | "indeterminate") => void
  ariaLabel?: string
}) {
  return (
    <Checkbox.Root
      checked={checked}
      onCheckedChange={onCheckedChange}
      aria-label={ariaLabel}
      className="flex size-4 items-center justify-center rounded-[4px] border border-neutral-7 bg-neutral-1 data-[state=checked]:bg-accent-9 data-[state=indeterminate]:bg-accent-9"
    >
      <Checkbox.Indicator className="text-white">
        <CheckIcon width={12} height={12} />
      </Checkbox.Indicator>
    </Checkbox.Root>
  )
}

// type Person = { id: number; name: string; age: number; city: string; note?: string };
// const columns: ColumnDef<Person>[] = [
//   { accessorKey: 'name', header: 'Jméno', cell: info => info.getValue(), },
//   { accessorKey: 'age', header: 'Věk', cell: info => info.getValue(), },
//   { accessorKey: 'city', header: 'Město', cell: info => info.getValue(), },
// ];

// function Demo() {
//   const data = React.useMemo<Person[]>(() => Array.from({length: 1000}).fill(0).map((_, i) => ({
//     id: i + 1,
//     name: `Person ${i+1}`,
//     age: 18 + (i % 50),
//     city: ['Prague','Brno','Ostrava','Pilsen'][i % 4]!,
//     note: i % 5 === 0 ? 'Expandable row' : undefined,
//   })), []);

//   return (
//       <DataTable
//         data={data}
//         columns={columns}
//         renderExpanded={(row) => (
//           <div className="text-sm text-neutral-11">Details: {row.note ?? 'No details'}</div>
//         )}
//       />
//   );
// }
