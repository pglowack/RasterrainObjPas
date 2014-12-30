unit uSimpleRayTracer;

interface

uses
  System.Generics.Collections,
  uVect, uRay, uCamera, uSphere, uPlane, uLight, uColour, uSource, uSceneObject;

type
  RGBType = record
    r, g, b: Double;
  end;

  TSimpleRayTracer = class
  private
    n, width, height: integer;
    antiAliasingLevel: integer;
    pixels: array of RGBType;
    function winningObjectIndex(intersections: TList<double>): integer;
    function getColorAt(intersection_position, intersecting_ray_direction: Vect;
      scene_objects: TObjectList<SceneObject>; index_of_winning_object: integer;
      light_sources: TObjectList<Source>; accuracy, ambientlight: Double): Colour;
  public
    constructor Create(aWidth, aHeight: integer; aAntiAliasingLevel: integer = 1);
    destructor Destroy; override;
    function getWidth: integer;
    function getHeight: integer;
    function getPixelData(x, y: integer): RGBType;
    procedure CalculatePixelColors;
  end;

implementation

uses
  System.Math;

{ TSimpleRayTracer }

constructor TSimpleRayTracer.Create(aWidth, aHeight: integer; aAntiAliasingLevel: integer = 1);
begin
  width := aWidth;
  height := aHeight;
  antiAliasingLevel := 1;
  // must be bigger then 0
  if aAntiAliasingLevel > 1 then
    antiAliasingLevel := aAntiAliasingLevel;

  n := width * height;
  SetLength(pixels, n);
end;

destructor TSimpleRayTracer.Destroy;
begin
  Finalize(pixels);
  inherited;
end;

function TSimpleRayTracer.getColorAt(intersection_position,
  intersecting_ray_direction: Vect; scene_objects: TObjectList<SceneObject>;
  index_of_winning_object: integer; light_sources: TObjectList<Source>;
  accuracy, ambientlight: Double): Colour;

var
  winning_object_color: Colour; winning_object_normal: Vect;
  square: integer; final_color: Colour; dot1: Double;
  scalar1, add1, scalar2, add2, reflection_direction: Vect;
  reflection_ray: Ray; reflection_intersections: TList<Double>;
  reflection_index, index_of_winning_object_with_reflection: integer;
  reflection_intersection_position, reflection_intersection_ray_direction: Vect;
  reflection_intersection_color: Colour;
  light_index: integer; light_direction: Vect; cosine_angle: Double;
  shadowed: boolean; distance_to_light: Vect; distance_to_light_magnitude: Double;
  shadow_ray: Ray; secondary_intersections: TList<Double>;
  object_index, c: integer;
  specular: Double;

begin
	winning_object_color := scene_objects.Items[index_of_winning_object].getColor();
	winning_object_normal := scene_objects.Items[index_of_winning_object].getNormalAt(intersection_position);

  if SameValue(winning_object_color.getColorSpecial(), 2, accuracy) then
  begin
		// checkered/tile floor pattern

    square := round(floor(intersection_position.x)) + round(floor(intersection_position.z));

    if square mod 2 = 0 then
    begin
			// black tile
      winning_object_color.setColorRed(0);
      winning_object_color.setColorGreen(0);
      winning_object_color.setColorBlue(0);
    end
    else
    begin
	  	// white tile
      winning_object_color.setColorRed(1);
      winning_object_color.setColorGreen(1);
      winning_object_color.setColorBlue(1);

    end;
  end;

  final_color := winning_object_color.colorScalar(ambientlight);

  if (winning_object_color.getColorSpecial() > 0) and (winning_object_color.getColorSpecial() <= 1) then
  begin
		// reflection from objects with specular intensity
		dot1 := winning_object_normal.dotProduct(intersecting_ray_direction.negative());
		scalar1 := winning_object_normal.vectMult(dot1);
		add1 := scalar1.vectAdd(intersecting_ray_direction);
		scalar2 := add1.vectMult(2);
		add2 := intersecting_ray_direction.negative().vectAdd(scalar2);
		reflection_direction := add2.normalize();

		reflection_ray := Ray.Create(intersection_position, reflection_direction);

		// determine what the ray intersects with first
    reflection_intersections := TList<Double>.Create;
    try
      for reflection_index := 0 to scene_objects.Count-1 do
        reflection_intersections.Add(scene_objects.Items[reflection_index].findIntersection(reflection_ray));

      index_of_winning_object_with_reflection := winningObjectIndex(reflection_intersections);

      if index_of_winning_object_with_reflection <> -1 then
      begin
        // reflection ray missed everthing else

        if reflection_intersections.Items[index_of_winning_object_with_reflection] > accuracy then
        begin
          // determine the position and direction at the point of intersection with the reflection ray
          // the ray only affects the color if it reflected off something

          reflection_intersection_position := intersection_position.vectAdd(reflection_direction.vectMult(reflection_intersections.Items[index_of_winning_object_with_reflection]));
          reflection_intersection_ray_direction := reflection_direction;

          reflection_intersection_color := getColorAt(reflection_intersection_position, reflection_intersection_ray_direction, scene_objects, index_of_winning_object_with_reflection, light_sources, accuracy, ambientlight);

          final_color := final_color.colorAdd(reflection_intersection_color.colorScalar(winning_object_color.getColorSpecial()));
        end;
      end;

    finally
      reflection_intersections.Free;
    end;

  end;

  for light_index := 0 to light_sources.Count-1 do
  begin
		light_direction := light_sources.Items[light_index].getLightPosition().vectAdd(intersection_position.negative()).normalize();
		cosine_angle := winning_object_normal.dotProduct(light_direction);

    if cosine_angle > 0 then
    begin
			// test for shadows

			shadowed := false;

			distance_to_light := light_sources.Items[light_index].getLightPosition().vectAdd(intersection_position.negative()).normalize();
			distance_to_light_magnitude := distance_to_light.magnitude();

			shadow_ray := Ray.Create(intersection_position, light_sources.Items[light_index].getLightPosition().vectAdd(intersection_position.negative()).normalize());

      secondary_intersections := TList<Double>.Create;
      try

        for object_index := 0 to scene_objects.Count-1 do
          secondary_intersections.Add(scene_objects.Items[object_index].findIntersection(shadow_ray));

        for c := 0 to secondary_intersections.Count-1 do
        begin
          if secondary_intersections.Items[c] > accuracy then
            if secondary_intersections.Items[c] <= distance_to_light_magnitude then
              shadowed := True;
        end;

      finally
        secondary_intersections.Free;
      end;

      if shadowed = false then
      begin
        final_color := final_color.colorAdd(winning_object_color.colorMultiply(light_sources.Items[light_index].getLightColor()).colorScalar(cosine_angle));

        if (winning_object_color.getColorSpecial() > 0) and (winning_object_color.getColorSpecial() <= 1) then
        begin
					// special [0-1]
					dot1 := winning_object_normal.dotProduct(intersecting_ray_direction.negative());
					scalar1 := winning_object_normal.vectMult(dot1);
					add1 := scalar1.vectAdd(intersecting_ray_direction);
					scalar2 := add1.vectMult(2);
					add2 := intersecting_ray_direction.negative().vectAdd(scalar2);
					reflection_direction := add2.normalize();

          specular := reflection_direction.dotProduct(light_direction);
          if specular > 0 then
          begin
            specular := Power(specular, 10);
            final_color := final_color.colorAdd(light_sources.Items[light_index].getLightColor().colorScalar(specular*winning_object_color.getColorSpecial()));
          end;

        end;

      end;

    end;
  end;

  Result := final_color.Clip;
end;

function TSimpleRayTracer.getHeight: integer;
begin
  Result := height;
end;

function TSimpleRayTracer.getPixelData(x, y: integer): RGBType;
begin
  Result := pixels[y * width + x];
end;

function TSimpleRayTracer.getWidth: integer;
begin
  Result := width;
end;

function TSimpleRayTracer.winningObjectIndex(
  intersections: TList<double>): integer;
var i, index_of_minimum_value: integer; aMax: Double;
begin
  // return the index of the winning intersection
  // prevent unnessary calculations
  if intersections.Count = 0 then
    // if there are no intersections
    Result := -1
  else if intersections.Count = 1 then
  begin
    if intersections.Items[0] > 0 then
      // if that intersection is greater than zero then its our index of minimum value
      Result := 0
    else
      // otherwise the only intersection value is negative
      Result := -1;
  end
  else
  begin
		// otherwise there is more than one intersection
		// first find the maximum value
    aMax := 0;
    for i := 0 to intersections.Count-1 do
      if aMax < intersections.Items[i] then
        aMax := intersections.Items[i];

		// then starting from the maximum value find the minimum positive value
    if aMax > 0 then
    begin
			// we only want positive intersections
      for i := 0 to intersections.Count-1 do
        if (intersections.Items[i] > 0) and (intersections.Items[i] <= aMax) then
        begin
          aMax := intersections.Items[i];
          index_of_minimum_value := i;
        end;
      Result := index_of_minimum_value;
    end
    else
			// all the intersections were negative
			Result := -1;
  end;
end;

procedure TSimpleRayTracer.CalculatePixelColors;
var xx, yy: integer; c: RGBType;
  avgRed, avgGreen, avgBlue: Double;
  floatWidth, floatHeight: Double;
  aadepth, aadepthSqr: integer; aathreshold, aspectratio, ambientlight, accuracy: Double;
  O, X, Y, Z: Vect;
  new_sphere_location, campos, look_at, diff_btw, camdir, camright, camdown: Vect;
  scene_cam: Camera; white_light, pretty_green, maroon, tile_floor, gray, black: Colour;
  light_position: Vect; scene_light: Light;
  light_sources: TObjectList<Source>; scene_objects: TObjectList<SceneObject>;
  scene_sphere, scene_sphere2: Sphere; scene_plane: Plane;
  thisone, aa_index: integer; xamnt, yamnt: Double;
  tempRed, tempGreen, tempBlue: array of Double;
  totalRed, totalGreen, totalBlue: Double;
  iRed, iGreen, iBlue: integer;
  aax, aay: integer;
  floatAax, floatAay, floatAadepth: Double;
  cam_ray_origin, cam_ray_direction: Vect;
  cam_ray: Ray;
  intersections: TList<Double>;
  index: integer;
  index_of_winning_object: integer;
  intersection_position, intersecting_ray_direction: Vect;
  intersection_color: Colour;
begin
//  aadepth := 1;
  aadepth := antiAliasingLevel;

  aadepthSqr := aadepth * aadepth;
  aathreshold := 0.1;
  floatWidth := width;
  floatHeight := height;
  aspectratio := floatWidth/floatHeight;
  ambientlight := 0.2;
  accuracy := 0.00000001;

  O := Vect.Create(0,0,0);
  X := Vect.Create(1,0,0);
  Y := Vect.Create(0,1,0);
  Z := Vect.Create(0,0,1);

  new_sphere_location := Vect.Create(1.75, -0.2, 0);

  campos := Vect.Create(3, 1.5, -4);

  look_at := Vect.Create(0,0,0);
  diff_btw := Vect.Create(campos.x - look_at.x, campos.y - look_at.y, campos.z - look_at.z);

  camdir := diff_btw.negative().normalize();
  camright := Y.crossProduct(camdir).normalize();
  camdown := camright.crossProduct(camdir);

  scene_cam := Camera.Create(campos, camdir, camright, camdown);

  white_light := Colour.Create(1.0, 1.0, 1.0, 0);
  pretty_green := Colour.Create(0.5, 1.0, 0.5, 0.3);
  maroon := Colour.Create(0.5, 0.25, 0.25, 0);
  tile_floor := Colour.Create(1, 1, 1, 2);
  gray := Colour.Create(0.5, 0.5, 0.5, 0);
  black := Colour.Create(0.0, 0.0, 0.0, 0);

  light_position := Vect.Create(-7,10,-10);
  scene_light := Light.Create(light_position, white_light);

  scene_sphere := Sphere.Create(O, 1, pretty_green);
  scene_sphere2 := Sphere.Create(new_sphere_location, 0.5, maroon);
  scene_plane := Plane.Create(Y, -1, tile_floor);

  light_sources := TObjectList<Source>.Create;
  scene_objects := TObjectList<SceneObject>.Create;
  SetLength(tempRed, aadepthSqr);
  SetLength(tempGreen, aadepthSqr);
  SetLength(tempBlue, aadepthSqr);
  try
    light_sources.Add(scene_light);

    scene_objects.Add(scene_sphere);
    scene_objects.Add(scene_sphere2);
    scene_objects.Add(scene_plane);

    for xx := 0 to width-1 do
      for yy := 0 to height-1 do
      begin
        thisone := yy*width + xx;

        for aax := 0 to aadepth-1 do
          for aay := 0 to aadepth-1 do
          begin
            aa_index := aay*aadepth + aax;

            // create the ray from the camera to this pixel
            if aadepth = 1 then
            begin
              // start with no anti-aliasing

              if width > height then
              begin
                // the image is wider than it is tall
							  xamnt := ((xx+0.5)/width)*aspectratio - (((width-height)/floatHeight)/2);
							  yamnt := ((height - yy) + 0.5)/height;
              end
              else if width < height then
              begin
                // the imager is taller than it is wide
							  xamnt := (xx + 0.5)/ width;
							  yamnt := (((height - yy) + 0.5)/height)/aspectratio - (((height - width)/floatWidth)/2);
              end
              else
              begin
                // the image is square
							  xamnt := (xx + 0.5)/width;
							  yamnt := ((height - yy) + 0.5)/height;
              end;

            end
            else
            begin
              // anti-aliasing

              floatAax := aax;
              floatAay := aay;
              floatAadepth := aadepth;

              if width > height then
              begin
                // the image is wider than it is tall
							  xamnt := ((xx + floatAax/(floatAadepth - 1))/width)*aspectratio - (((width-height)/floatHeight)/2);
							  yamnt := ((height - yy) + floatAax/(floatAadepth - 1))/height;
              end
              else if width < height then
              begin
                // the imager is taller than it is wide
							  xamnt := (xx + floatAax/(floatAadepth - 1))/ width;
							  yamnt := (((height - yy) + floatAax/(floatAadepth - 1))/height)/aspectratio - (((height - width)/floatWidth)/2);
              end
              else
              begin
                // the image is square
							  xamnt := (xx + floatAax/(floatAadepth - 1))/width;
							  yamnt := ((height - yy) + floatAax/(floatAadepth - 1))/height;
              end;
            end;

					  cam_ray_origin := scene_cam.getCameraPosition();
					  cam_ray_direction := camdir.vectAdd(camright.vectMult(xamnt - 0.5).vectAdd(camdown.vectMult(yamnt - 0.5))).normalize();

					  cam_ray := Ray.Create(cam_ray_origin, cam_ray_direction);

            intersections := TList<Double>.Create;
            try
              for index := 0 to scene_objects.Count-1 do
                intersections.Add(scene_objects.Items[index].findIntersection(cam_ray));

              index_of_winning_object := winningObjectIndex(intersections);

              if index_of_winning_object = -1 then
              begin
                // set the backgroung black
                tempRed[aa_index] := 0;
                tempGreen[aa_index] := 0;
                tempBlue[aa_index] := 0;
              end
              else if intersections.Items[index_of_winning_object] > accuracy then
              begin
							  intersection_position := cam_ray_origin.vectAdd(cam_ray_direction.vectMult(intersections[index_of_winning_object]));
							  intersecting_ray_direction := cam_ray_direction;

							  intersection_color := getColorAt(intersection_position, intersecting_ray_direction, scene_objects, index_of_winning_object, light_sources, accuracy, ambientlight);

							  tempRed[aa_index] := intersection_color.getColorRed();
							  tempGreen[aa_index] := intersection_color.getColorGreen();
							  tempBlue[aa_index] := intersection_color.getColorBlue();
              end;

            finally
              intersections.Free;
            end;

          end;

        // average the pixel color
        totalRed := 0;
        totalGreen := 0;
        totalBlue := 0;

        for iRed := 0 to aadepthSqr-1 do
          totalRed := totalRed + tempRed[iRed];

        for iGreen := 0 to aadepthSqr-1 do
          totalGreen := totalGreen + tempGreen[iGreen];

        for iBlue := 0 to aadepthSqr-1 do
          totalBlue := totalBlue + tempRed[iBlue];

			  avgRed := totalRed/(aadepthSqr);
			  avgGreen := totalGreen/(aadepthSqr);
			  avgBlue := totalBlue/(aadepthSqr);

        c.r := avgRed;
        c.g := avgGreen;
        c.b := avgBlue;

        pixels[thisone] := c;
      end;

  finally
    light_sources.Free;
    scene_objects.Free;
    Finalize(tempRed);
    Finalize(tempGreen);
    Finalize(tempBlue);
  end;
end;

end.
