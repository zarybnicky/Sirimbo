import { useCurrentTenantQuery } from "lib/graphql/Tenant";
import { SlateReadonly } from "./SlateReadonly";

export function TenantInformation() {
  const { data: tenant } = useCurrentTenantQuery();
  const data = tenant?.getCurrentTenant;
  if (!data) {
    return null;
  }

  return <div className="flex flex-col items-center">
    <h3 className="text-2xl tracking-wide">{data.name}</h3>

    <div className="w-full px-4">
      <p>Sídlo spolku/společnosti</p>
      <p>Statutární orgány</p>
      <p>Pobočky, sály na nich</p>
      <p>Platební informace</p>
      <SlateReadonly value={data.memberInfo as any[]} />
    </div>

    <h3 className="mt-4 text-2xl tracking-wide">Místa</h3>
    {data.locationsByTenant.nodes.map(item => (
      <div key={item.id}>
        <h4 className="text-xl tracking-wide">{item.name}</h4>
        <SlateReadonly value={item.description as any[]} />
      </div>
    ))}
  </div>;
}
