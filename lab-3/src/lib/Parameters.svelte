<script lang="ts">
  import {
    Form,
    FormGroup,
    Checkbox,
    Select,
    SelectItem,
    NumberInput,
  } from "carbon-components-svelte";
  import type { InterpolationType } from "./types";

  export const methods: InterpolationType[] = [
    { id: "linear", name: "Линейная интерполяция", displayName: "Linear" },
    {
      id: "lagrange",
      name: "Интерполяция методом Лагранжа",
      displayName: "Lagrange method",
    },
    {
      id: "newton",
      name: "Интерполяция методом Ньютона",
      displayName: "Newton method",
    },
    {
      id: "rbf",
      name: "Интерполяция методом RBF",
      displayName: "Radial basis function method",
    },
  ];

  export let locked = false;
  export let selectedMethods: boolean[] = [true];
  export let selectedGraph: string = "default";
  export let step: number = 1;

  export const reset = () => {
    step = 1;
    selectedMethods = [true];
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
          disabled={(selectedMethods.reduce(
            (acc, val) => acc + (val ? 1 : 0),
            0
          ) === 1 &&
            selectedMethods[index]) ||
            locked}
        />
      {/each}
      <br />
      <NumberInput
        step={0.1}
        min={0.1}
        bind:value={step}
        label="Шаг интерполяции"
        disabled={locked}
      />
      <br />
      <Select labelText="Отображаемый график" bind:selected={selectedGraph}>
        <SelectItem disabled hidden value="default" text="По умолчанию" />
        {#each methods as method, index (method.id)}
          {#if selectedMethods[index]}
            <SelectItem value={method.id} text={method.name} />
          {/if}
        {/each}
      </Select>
    </FormGroup>
  </Form>
</section>
