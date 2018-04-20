# raytracer

```
stack build
stack exec raytracer-exe +RTS -N4
```

# step 1
* has a ray to sphere intersection detection
* output image to jpeg
![Step 1](./images/step1.png "Step 1")

# step 2
* add image dimension as input
* add multiple spheres
![Step 2](./images/step2.png "Step 2")

# step 3
* add a light source and compute lambert lighting
* replace `Vec` with `linear` library for vector maths
![Step 3](./images/step3.png "Step 3")

# step 4
* add material colours and light sources
![Step 4](./images/step4.png "Step 4")

# step 5
* add multiple light sources
* add material diffusion
![Step 5](./images/step5.png "Step 5")

# step 6
* add reflection
![Step 6](./images/step6.png "Step 6")

# step 7
* refactor code
* complete with lambertian + Blinn–Phong shading model
![Step 7](./images/step7.png "Step 7")

# step 8
* add shadow
![Step 8](./images/step8.png "Step 8")

# step 9
* add antialiasing
* add plane
![Step 9](./images/step9.png "Step 9")
