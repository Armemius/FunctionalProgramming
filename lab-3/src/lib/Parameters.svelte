<script lang="ts">
  import {
    Form,
    FormGroup,
    Checkbox,
    Select,
    SelectItem,
  } from "carbon-components-svelte";
  import type { InterpolationType } from "./types";

  const methods: InterpolationType[] = [
    { id: "linear", name: "Линейная интерполяция" },
    {
      id: "lagrange",
      name: "Интерполяция методом Лагранжа",
    },
    {
      id: "newton",
      name: "Интерполяция методом Ньютона",
    },
    {
      id: "spline",
      name: "Интерполяция сплайнами",
    },
  ];

  export let locked = false;
  export let selectedMethods: boolean[] = [true];
  export let selectedGraph: string = "default";

  export const reset = () => {
    selectedMethods = [true]
    selectedGraph = "default";
  };
</script>

<section>
  <h3 class="pb-1">Параметры</h3>
  <Form on:submit>
    <FormGroup legendText="Методы интерполяции">
      {#each methods as method, index (method.id)}
        <Checkbox
          id={method.id}
          name="methods"
          labelText={method.name}
          bind:checked={selectedMethods[index]}
          disabled={(selectedMethods.reduce((acc, val) => acc + val, 0) === 1
            && selectedMethods[index])
            || locked}
        />
      {/each}
    </FormGroup>
    <FormGroup>
      <Select labelText="Отображаемый график" bind:selected={selectedGraph}>
        <SelectItem
          disabled
          hidden
          value="default"
          text="По умолчанию"
        />
        {#each methods as method, index (method.id)}
          {#if selectedMethods[index]}
            <SelectItem value={method.id} text={method.name} />
          {/if}
        {/each}
      </Select>
    </FormGroup>
  </Form>
</section>
